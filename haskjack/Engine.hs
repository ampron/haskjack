
module Engine
( Result (Done, Input, Next)
, Engine (..)
) where

data Result a = Done a | Input (IO String, String -> a) | Next (IO a)

class Engine a where
    -- defined by instance
    transition :: a -> Result a
    -- defined by class
    run :: a -> IO a
    run x = case transition x of
        Done x' -> return x'
        Input (action, continuator) -> do
            response <- action
            run (continuator response)
        Next action -> do
            x' <- action
            run x'
