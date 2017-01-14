
-- Local
import Engine
import Cards
import RandMonad

-- Standard
import Data.Maybe
import System.Random

shuffledDeck :: (RandomGen g) => Ranad g [Card]
shuffledDeck = shuffle freshDeck

-- ========================================================== --
data PlayerAction = Stay | Hit deriving Show

readHit :: String -> PlayerAction
readHit "Hit" = Hit
readHit "hit" = Hit
readHit "yes" = Hit
readHit "y"   = Hit
readHit _ = Stay

getAction :: IO PlayerAction
getAction = fmap readHit $ do
    putStrLn "Hit?"
    getLine
    
-- ========================================================== --
data Outcome = Win | Push | Loss

-- ========================================================== --
data Stage = Deal | Play Int | Completed deriving Show

data GameTable = GameTable { stage :: Stage
                           , deck :: [Card]
                           , dealer :: [Card]
                           , player :: [Card]
                           }

instance Show GameTable where
    show (GameTable stg _ dhand phand) =
        let dScore = scoreCards dhand
            pScore = scoreCards phand
            pretty card = "[" ++ show card ++ "]"
            secretCard = "[?]"
            dealerShow [] = ""
            dealerShow [c] = case stg of
                Deal      -> secretCard
                Play 0    -> secretCard
                Play (-1) -> pretty c ++ " -> " ++ show dScore
                Completed -> pretty c ++ " -> " ++ show dScore
            dealerShow (c:cs) = pretty c ++ ", " ++ dealerShow cs
            playerShow [] = ""
            playerShow [c] = pretty c ++ " -> " ++ show pScore
            playerShow (c:cs) = pretty c ++ ", " ++ playerShow cs
        in show stg ++ "\n" ++
           "  Dealer: " ++ dealerShow dhand ++ "\n" ++
           "  Player: " ++ playerShow phand

tableWithDeck :: [Card] -> GameTable
tableWithDeck newDeck = GameTable Deal newDeck [] []

scorePlayer :: GameTable -> Int
scorePlayer = scoreCards . player

scoreDealer :: GameTable -> Int
scoreDealer = scoreCards . dealer

winner :: GameTable -> Outcome
winner tbl
    | (21 < dscore) && (21 < pscore) = Push
    | 21 < dscore = Win
    | 21 < pscore = Loss
    | dscore <  pscore = Win
    | dscore == pscore = Push
    | dscore >  pscore = Loss
    | otherwise = Push -- unreachable
    where dscore = scoreDealer tbl
          pscore = scorePlayer tbl
          
-- ========================================================== --
dealToPlayer :: Int -> GameTable -> GameTable
dealToPlayer n (GameTable stg dk dhand phand) =
    let ([card], dk') = splitAt n dk
    in GameTable stg dk' dhand (card : phand)

dealToDealer :: Int -> GameTable -> GameTable
dealToDealer n (GameTable stg dk dhand phand) =
    let ([card], dk') = splitAt n dk
    in GameTable stg dk' (card : dhand) phand

dealOutCards :: GameTable -> GameTable
dealOutCards (GameTable _ dk _ _) =
    let (dealerHand, dk') = splitAt 2 dk
        (playerHand, dk'') = splitAt 2 dk'
    in GameTable (Play 0) dk'' dealerHand playerHand

stepGame :: GameTable -> IO GameTable
-- Player's turn
stepGame (GameTable (Play 0) dk dhand phand)
    | 21 <= scoreCards phand = return $ GameTable (Play (-1)) dk dhand phand
    | otherwise =
        let effect Hit  =
                let ([nextCard], dk') = splitAt 1 dk
                in GameTable (Play 0) dk' dhand (nextCard : phand)
            effect Stay = GameTable (Play (-1)) dk dhand phand
        in fmap effect getAction
-- Dealer's turn
stepGame (GameTable (Play (-1)) dk dhand phand)
    | 16 <= scoreCards dhand = return $ GameTable Completed dk dhand phand
    | otherwise =
        let ([nextCard], dk') = splitAt 1 dk
        in return $ GameTable (Play (-1)) dk' (nextCard : dhand) phand

instance Engine GameTable where
    transition tbl = case stage tbl of
                         Deal -> Next $ print tbl *> return (dealOutCards tbl)
                         Play _ -> Next $ print tbl *> stepGame tbl
                         Completed -> Done tbl
                         
-- ========================================================== --
data GamePhase = GameOver | OutOfPlay | InPlay GameTable

data GameState g = GameState { rstream :: g
                             , wallet :: Int
                             , phase :: GamePhase
                             }

instance (RandomGen g) => Engine (GameState g) where
    -- Game's over exit
    transition gm@(GameState _ _ GameOver) = Done gm
    -- A round is over, ask to play again
    transition gm@(GameState _ 0 OutOfPlay) = Done gm
    transition gm@(GameState _ _ OutOfPlay) =
        let continuator response = if isYes response then startNewGame gm
                                                     else endGame gm
            action = do
                    putStrLn "\n***\nWould you like to play again?\n***\n"
                    response <- getLine
                    return $ continuator response
        in Next action
    -- Start a round
    transition (GameState rgen w (InPlay tbl)) =
        let prompt = "How much would you like to wager? (You have $" ++ show w ++ ")"
            action = do
                putStrLn prompt
                -- response <- getLine
                wager <- getWager w
                putStrLn $ "You wagered $" ++ show wager
                -- let wager = read response :: Int
                finalTbl <- run tbl
                let result = winner finalTbl
                putStrLn $ case result of
                               Win -> "You win! :-)"
                               Push -> "Push :-|"
                               Loss -> "You lose :'-("
                let w' = case result of
                             Win -> w + wager
                             Push -> w
                             Loss -> w - wager
                putStrLn $ "You now have $" ++ show w'
                return $ GameState rgen w' OutOfPlay
        in Next action

isYes :: String -> Bool
isYes "y" = True
isYes _ = False

endGame :: GameState g -> GameState g
endGame gm = GameState (rstream gm) (wallet gm) GameOver

startNewGame :: (RandomGen g) => GameState g -> GameState g
startNewGame gm =
    let (newTbl, rgen') = applyGen (rstream gm) (fmap tableWithDeck shuffledDeck)
    in GameState rgen' (wallet gm) (InPlay newTbl)

tryRead :: (Read a) => String -> Either String a
tryRead cs = case fmap fst (listToMaybe $ reads cs) of
                   Just n -> Right n
                   Nothing -> Left $ "could not parse \"" ++ cs ++ "\""

tryWithin :: (Ord a, Show a) => (a, a) -> a -> Either String a
tryWithin (a, b) x
    | (a < x) && (x <= b) = Right x
    | otherwise           = Left $ "value of " ++ show x ++ " was out of range (" ++ show a ++ ", " ++ show b ++ "]"

getWager :: Int -> IO Int
getWager maxWager =
    let checkResponse = tryWithin (0, maxWager)
        -- readWager :: String -> Maybe Int
        readWager s = tryRead s >>= checkResponse
    in do
        response <- getLine
        case readWager response of
            Right x -> return x
            Left errMsg -> do
                putStrLn errMsg
                getWager maxWager
                
-- ========================================================== --
main :: IO ()
main = do
    rgen <- getStdGen
    putStrLn ""
    putStrLn "$~$~$~$~$~$~$~$~$~$~$~$~$~$~$~$~$~$"
    putStrLn "$ Welcome to the Haskell Casino!  $"
    putStrLn "$~$~$~$~$~$~$~$~$~$~$~$~$~$~$~$~$~$"
    putStrLn " debug version 2"
    putStrLn ""
    let initState = startNewGame $ GameState rgen 100 GameOver
    _ <- run initState
    return ()
