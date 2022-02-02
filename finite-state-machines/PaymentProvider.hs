{-# LANGUAGE OverloadedStrings #-}

-- | Dummy module for demonstrative purposes.
module PaymentProvider where

import Checkout
import Control.Monad.IO.Class
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T

chargeCard :: MonadIO m => Card -> Price -> m ()
chargeCard (Card card) price =
  liftIO
    ( T.putStrLn
        ( "Charging card " <> card <> " $"
            <> T.pack (show price)
        )
    )