{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Checkout
  ( Card (..),
    CartItem,
    OrderId (..),
  )
import qualified Checkout
import qualified ConsoleInput
import qualified Control.Monad.IO.Class as IO.Class
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Semigroup as Semigroup
import qualified Data.Text.IO as Text.IO
import qualified PaymentProvider

foo :: Char
foo = 2
