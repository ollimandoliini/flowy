{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where
import Control.Monad.Cont
import GHC.Conc (TVar)
import Data.IntMap (IntMap)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Fix (MonadFix)
import Data.Aeson (ToJSON (toJSON), Value (String))
import GHC.Generics (Generic)

type WorkflowState = Maybe (IntMap TaskStatus)

newtype ExecutionState a = ExecutionState {
  state :: TVar (Maybe (IntMap TaskStatus))
}

newtype App m env a = App
  { runApp :: ReaderT (ExecutionState env) m a }
  deriving (
      Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadFix
      , MonadReader (ExecutionState env)
      )


data Task = Task {
    id :: Int
    , status :: TaskStatus
} deriving (Generic, Show)

data TaskStatus
    = Completed
    | Running
    | Pending
    deriving (Show)



newtype WorkFlowStatus = WorkFlowStatus {
    tasks :: [Task]
    } deriving (Show, Generic)

instance ToJSON WorkFlowStatus
instance ToJSON Task

    

instance ToJSON TaskStatus where
    toJSON Completed = String "completed"
    toJSON Running = String "running"
    toJSON Pending = String "pending"

