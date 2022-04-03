{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import Servant
import Types (WorkFlowStatus (WorkFlowStatus), WorkflowState, Task (Task))
import GHC.Conc (readTVarIO, TVar)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.IntMap (toList)


type TaskAPI = Get '[JSON] WorkFlowStatus

workFlowToAPI :: WorkflowState -> IO WorkFlowStatus
workFlowToAPI Nothing = return $ WorkFlowStatus []
workFlowToAPI (Just wf) =
    return $ WorkFlowStatus $ uncurry Task <$> toList wf

        
server :: TVar WorkflowState -> Handler WorkFlowStatus
server tvar = do
    work <- liftIO $ readTVarIO tvar
    liftIO $ workFlowToAPI work

app :: TVar WorkflowState -> Application
app tvar = serve (Proxy :: Proxy TaskAPI) (server tvar)

