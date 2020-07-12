{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Elevator.Safe.Primitive where

import Data.Type.Nat
import Data.Singletons.TH
import Control.Monad.Trans

import qualified Elevator.LowLevel as LL
import Elevator.Safe.Floor

$(singletons [d|
 data Door = Opened | Closed
  deriving (Eq, Show)
  |])

data Elevator (mx :: Nat) (cur :: Nat) (door :: Door) where
  MkElevator :: SingI door => Floor mx cur -> Elevator mx cur door

instance Show (Elevator mx cur door) where
  show (MkElevator fl@MkFloor) =
    "Elevator {current = " <> show fl
    <> ", door = " <> show (fromSing (sing :: Sing door)) <> "}"

currentFloor :: Elevator mx cur door -> Floor mx cur
currentFloor (MkElevator fl) = fl

up :: (BelowTop mx cur, MonadIO m) =>
      Elevator mx cur Closed -> m (Elevator mx (S cur) Closed)
up (MkElevator fl) = do
  LL.up
  pure (MkElevator $ next fl)

down :: MonadIO m => Elevator mx (S cur) Closed -> m (Elevator mx cur Closed)
down (MkElevator fl) = do
  LL.down
  pure $ MkElevator $ prev fl

open :: MonadIO m =>
        Floor mx cur -> Elevator mx cur Closed -> m (Elevator mx cur Opened)
open _ (MkElevator fl) = do
  LL.open
  pure (MkElevator fl)

close :: MonadIO m =>
         Floor mx cur -> Elevator mx cur Opened -> m (Elevator mx cur Closed)
close _ (MkElevator fl) = do
  LL.close
  pure (MkElevator fl)

ensureClosed :: forall mx cur door m. MonadIO m =>
                Elevator mx cur door -> m (Elevator mx cur Closed)
ensureClosed el@(MkElevator fl) =
  case sing :: Sing door of
    SClosed -> pure el
    SOpened -> close fl el

ensureOpenedAt :: forall mx cur door m. MonadIO m =>
  Floor mx cur -> Elevator mx cur door -> m (Elevator mx cur Opened)
ensureOpenedAt fl el@(MkElevator _) =
  case sing :: Sing door of
    SOpened -> pure el
    SClosed -> open fl el
