module Types.WebAppError where

import           Control.Exception       (Exception)
import           Data.Typeable           (Typeable)

data WebAppError = WebAppError String deriving (Show, Typeable)

instance Exception WebAppError