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
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IO.Class
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Semigroup as Semigroup
import qualified Data.Text.IO as Text.IO
import qualified PaymentProvider
import Data.Kind

data NoItems
data HasItems
data NoCard
data CardSelected
data CardConfirmed
data OrderPlaced

class Checkout m where
  type State m :: Type -> Type
  initial :: m (State m NoItems)
  select :: SelectState m -> CartItem -> m (State m HasItems)
  checkout :: State m HasItems -> m (State m NoCard)
  cancel :: CancelState m -> m (State m HasItems)
  selectCard :: State m NoCard -> Card -> m (State m CardSelected)
  confirm :: State m CardSelected -> m (State m CardConfirmed)
  placeOrder :: State m CardConfirmed -> m (State m OrderPlaced)
  end :: State m OrderPlaced -> m OrderId

--  initial :: m NoItems
--  select :: SelectState -> CartItem -> m HasItems
--  checkout :: HasItems -> m NoCard
--  cancel :: CancelState -> m HasItems
--  selectCard :: NoCard -> Card -> m CardSelected
--  confirm :: CardSelected -> m CardConfirmed
--  placeOrder :: CardConfirmed -> m OrderPlaced
--  end :: OrderPlaced -> m OrderId

-- data SelectState m
--   = NoItemsSelect (State m NoItems)
--   | HasItemsSelect (State m HasItems)

data SelectState m
  = NoItemsSelect (State m NoItems)
  | HasItemsSelect (State m HasItems)

data CancelState m
  = NoCardCancel (State m NoCard)
  | CardSelectedCancel (State m CardSelected)
  | CardConfirmedCancel (State m CardConfirmed)

fillCart
  :: (Checkout m, MonadIO m)
  => State m NoItems
  -> m (State m HasItems)
fillCart noItems = do
  cartItem <- Checkout.mkItem <$> ConsoleInput.prompt "First item:"
  items <- select (NoItemsSelect noItems) cartItem
  selectMoreItems items

selectMoreItems
  :: (Checkout m, MonadIO m)
  => State m HasItems
  -> m (State m HasItems)
selectMoreItems s = do
  more <- ConsoleInput.confirm "More items?"
  if more
    then
      Checkout.mkItem <$> ConsoleInput.prompt "Next item:"
      >>= select (HasItemsSelect s)
      >>= selectMoreItems
    else return s

startCheckout
  :: (Checkout m, MonadIO m)
  => State m HasItems
  -> m (State m OrderPlaced)
startCheckout hasItems = do
  noCard <- checkout hasItems
  card <- ConsoleInput.prompt "Card:"
  cardSelected <- selectCard noCard (Card card)
  useCard <- ConsoleInput.confirm ("Confirm use of '" <> card <> "'?")
  if useCard
    then confirm cardSelected >>= placeOrder
    else cancel (CardSelectedCancel cardSelected) >>=
         selectMoreItems >>=
         startCheckout

checkoutProgram
  :: (Checkout m, MonadIO m)
  => m OrderId
checkoutProgram =
  initial >>= fillCart >>= startCheckout >>= end

-----------
data CheckoutState s where
-- data NoItems
  NoItems :: CheckoutState NoItems
-- data HasItems
  HasItems :: NonEmpty CartItem -> CheckoutState HasItems
-- data NoCard
  NoCard :: NonEmpty CartItem -> CheckoutState NoCard
-- data CardSelected
  CardSelected :: NonEmpty CartItem -> Card -> CheckoutState CardSelected
-- data CardConfirmed
  CardConfirmed :: NonEmpty CartItem -> Card -> CheckoutState CardConfirmed
-- data OrderPlaced
  OrderPlaced :: OrderId -> CheckoutState OrderPlaced


newtype CheckoutT m a = CheckoutT
  { runCheckoutT :: m a
  } deriving ( Functor
             , Monad
             , Applicative
             , MonadIO
             )

instance (MonadIO m) => Checkout (CheckoutT m) where
  type State (CheckoutT m) = CheckoutState
  initial = pure NoItems
  select state item = case state of
                        NoItemsSelect NoItems -> pure (HasItems (item :| []))
                        HasItemsSelect (HasItems items) -> pure (HasItems (item <| items))
  checkout (HasItems items) = pure (NoCard items) 
  cancel state = case state of
                   NoCardCancel (NoCard items) -> pure (HasItems items)
                   CardSelectedCancel (CardSelected items _) -> pure (HasItems items)
                   CardConfirmedCancel (CardConfirmed items _) -> pure (HasItems items)
  selectCard (NoCard items) card = pure (CardSelected items card)
  confirm (CardSelected items card) = pure (CardConfirmed items card)

  placeOrder (CardConfirmed _items card) = do -- TODO: Do something with items (like charge amount to card)
    orderId <- Checkout.newOrderId
    pure (OrderPlaced orderId)

  end (OrderPlaced orderId) = pure orderId
