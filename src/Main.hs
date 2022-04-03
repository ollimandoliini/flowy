module Main where
import GHC.Conc (newTVarIO)
import Control.Concurrent.Async (withAsync, wait)
import Runner
import Types
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad ( void )
import Network.Wai.Handler.Warp
import Api (app)


main :: IO ()
main = do
  tvar <- newTVarIO Nothing
  let env = ExecutionState tvar
  withAsync ((runReaderT . runApp) (runWorkflow steps) env) $ \a -> do
    run 8080 (app tvar)
    void $ wait a
