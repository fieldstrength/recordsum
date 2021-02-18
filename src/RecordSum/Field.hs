{-# LANGUAGE TypeApplications, KindSignatures, RankNTypes, DataKinds, MultiParamTypeClasses,
    TypeSynonymInstances, FlexibleInstances, LambdaCase, ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module RecordSum.Field (RecordSumField (..)) where

import Control.Lens hiding (to, from)
import Data.Generics.Product.Fields (HasField' (..))
import Data.Kind (Type)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits


class RecordSumField (field :: Symbol) a s | s field -> a where
  recordSumField :: Lens' s a

instance (Generic s, GRecordSumField field a (Rep s)) => RecordSumField field a s where
  recordSumField =
    lens
      (view (gRecordSumField @field) . from)
      (\s1 a1 -> to $ set (gRecordSumField @field) a1 $ from s1)


-------------------- internal classes --------------------

class GRecordSumField (field :: Symbol) (a :: Type) (repS :: Type -> Type) | field repS -> a where
  gRecordSumField :: Lens' (repS x) a

instance forall field s a meta metaSel. HasField' field s a
  => GRecordSumField field a (C1 meta (S1 metaSel (Rec0 s))) where
  gRecordSumField = miso . miso . kiso . (field' @field @s @a)

instance (GRecordSumField field a s, GRecordSumField field a t)
  => GRecordSumField field a (s :+: t) where
  gRecordSumField f = \case
    L1 l -> L1 <$> (gRecordSumField @field) f l
    R1 r -> R1 <$> (gRecordSumField @field) f r

instance GRecordSumField field a s
  => GRecordSumField field a (D1 meta s) where
  gRecordSumField = miso . (gRecordSumField @field)


-------------------- utilz --------------------

miso :: Iso' (M1 i c f p) (f p)
miso = iso unM1 M1

kiso :: Iso' (K1 i c p) c
kiso = coerced


-------------------- test --------------------

data RXY
  = MkX X
  | MkY Y
  deriving (Generic)

data X = X
  { foo :: Int
  , bar :: Char
  }
  deriving (Show, Eq, Generic)

data Y = Y
  { foo :: Int }
  deriving (Show, Eq, Generic)

got :: RXY -> Int
got = view $ recordSumField @"foo"

sot :: RXY -> Int -> RXY
sot rxy n = set (recordSumField @"foo") n rxy
