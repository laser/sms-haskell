module Types.JSONDecodeError where

import           Control.Exception       (Exception)
import           Data.Typeable           (Typeable)

data JSONDecodeError = JSONDecodeError String deriving (Show, Typeable)

instance Exception JSONDecodeError