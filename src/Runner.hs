{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Runner where

import Control.Concurrent.Async
import Control.Concurrent
import GHC.Conc
import Control.Monad.Fix (MonadFix(mfix))
import Data.IntMap ( (!?), fromList, toList, IntMap, adjust )
import Data.Maybe (mapMaybe)
import Control.Monad.Reader (MonadIO (liftIO), asks)
import Types (App, ExecutionState (state), TaskStatus (Pending, Running, Completed))
import Control.Concurrent.STM.TVar ( modifyTVar )


waitAllSTM :: [Async a] -> STM [a]
waitAllSTM [] = pure []
waitAllSTM (x:xs) = (:) <$> (waitSTM x `orElse` (waitAllSTM xs *> retry)) <*> waitAllSTM xs


waitAll :: [Async a] -> IO [a]
waitAll = atomically . waitAllSTM


data Task a = Task {
  id :: Int
  , dependencies :: [Int]
  , runTask :: IO a
}

step1 = Task 1 [] (threadDelay 10000000)
step2 = Task 2 [4] (threadDelay 10000000)
step3 = Task 3 [4] (threadDelay 10000000)
step4 = Task 4 [2, 3] (threadDelay 10000000)

steps = fromList $ zip [1..] [step4, step2, step3, step1]


runWorkflow :: IntMap (Task a) -> App IO a [a]
runWorkflow tasks' = do
    tvar <- asks state
    liftIO $ atomically $ modifyTVar tvar (const (Just $ Pending <$ tasks'))
    asyncs <- mfix $ \asyncs -> traverse (launch tvar asyncs) tasks'
    liftIO $ waitAll $ snd <$> toList asyncs
  where
    launch :: TVar (Maybe (IntMap TaskStatus)) -> IntMap (Async a) -> Task a -> App IO a (Async a)
    launch tvar asyncs (Task id' deps run) =
        liftIO $ async $ do
          _ <- waitAll (mapMaybe (asyncs !?) deps)
          atomically $ modifyTVar tvar (fmap $ adjust (const Running) id')
          result <- run
          atomically $ modifyTVar tvar (fmap $ adjust (const Completed) id')
          return result
    
    


      


