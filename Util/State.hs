
module Util.State where
import Control.Monad.State
    
fresh :: (Int -> a) -> State Int a
fresh f = do
    x <- get;
    put (x+1);
    return (f x)
