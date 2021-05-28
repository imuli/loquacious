{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Text.Loquacious
Description : Overridable Pretty Printed Values with Generic Annotations
Stability   : experimental

Loquacious enables pretty printing of values with the ability to override
individual typeclass instances in particular context, for example to provide
locali[sz]ation.
-}

module Text.Loquacious
  ( -- * Loquacious class and existential type.
    Loquacious(..)
  , Loq(..)
    -- * Handle Loqs, in particular to override them.
  , defLoq
  , LoqHandler(..)
  , loqHandle
  , loqHandles
  , overLoq
    -- * Helpers for polymorphic Loquacious instances
  , toLoqFunctor
  , fromLoqTraversable
  , toLoqBifunctor
  , fromLoqBitraversable
  , toLoqFirst
  , fromLoqFirst
  ) where

-- For core functionality.
import           Data.Maybe (fromMaybe)
import           Data.Text.Prettyprint.Doc (Doc, Pretty)
import           Data.Typeable (Typeable, cast)
import           Text.Loquacious.Annotations (Ann)

-- For helpers.
import           Data.Bifunctor (Bifunctor, bimap, first)
import           Data.Bitraversable (Bitraversable, bitraverse)
import           Data.Foldable (toList)
import           Data.Text.Prettyprint.Doc (align, colon, list, parens, pretty, tupled, viaShow,
                     vsep, (<+>))

-- For instances.
import qualified Control.Exception as E
import qualified Control.Exception.Base as EB
import           Data.Array.Unboxed (Array, IArray, Ix, UArray, elems)
import           Data.Complex (Complex((:+)))
import           Data.Functor.Const (Const, getConst)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Ratio (Ratio, denominator, numerator)
import           Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import           Data.Typeable (Proxy(Proxy), TypeRep, typeOf, typeRep, (:~:), (:~~:))
import           Data.Version (Version, showVersion)
import           Data.Void (Void)
import           Data.Word (Word16, Word32, Word64, Word8)
import           GHC.Stack
import           Numeric.Natural (Natural)

-- | The root of the Loquacious Type Heirarchy, compare 'E.SomeException'.
data Loq = forall t. Loquacious t => Loq t

-- | Construct a 'Doc' with the default loq handlers (those specified in
-- the typeclass bodies).
defLoq :: Loquacious s => s -> Doc Ann
defLoq = loq defLoq

instance Show Loq where
  show = show . defLoq

instance Loquacious Loq where
  loq m (Loq x) = m x
  toLoq = id
  fromLoq = pure

-- | Types than can be turned into Loquaciouss.
class Typeable t => Loquacious t where
  -- | Produce a 'Doc'ument ready for pretty-printing that may or may not
  -- include annotations.
  --
  -- For base types this can simply be 'pretty', but higher kinded types should
  -- implement this manually.
  loq :: (forall s. Loquacious s => s -> Doc Ann) -> t -> Doc Ann
  default loq :: Pretty t => (forall s. Loquacious s => s -> Doc Ann) -> t -> Doc Ann
  loq _ = pretty

  -- | Embed a 'Loquacious' as a 'Loq'.
  --
  -- Higher Kinded Types should recursively encode their arguments! This allows
  -- polymorphic deconstruction of 'Loq'.
  --
  -- For 'Functor's and 'Bifunctor's use 'toLoqFunctor' and 'toLoqBifunctor',
  -- respectively.
  toLoq :: t -> Loq
  toLoq = Loq

  -- | Attempt to extract some 'Loquacious' from a 'Loq'.
  --
  -- For 'Data.Traversable.Traverable's and 'Data.Bitraversable.Bitraverable's
  -- use 'fromLoqTraversable' and 'fromLoqBitraversable', respectively.
  fromLoq :: Loq -> Maybe t
  fromLoq (Loq sm) = cast sm

-- | Wrap a function from some 'Loquacious' type.
data LoqHandler t = forall m. Loquacious m => LoqHandler (m -> t)

-- | Given some loq and a handler for some loq, apply the handler if
-- possible, otherwise return a default value.
loqHandle :: Loq -> LoqHandler t -> t -> t
loqHandle m (LoqHandler handle) def = fromMaybe def (handle <$> fromLoq m)

-- | Given some loq and a bunch of handlers for some loq, apply each
-- handler in turn if possible, otherwise return a default value.
loqHandles :: Foldable f => Loq -> t -> f (LoqHandler t) -> t
loqHandles m def hs = foldr (loqHandle m) def hs

-- | Override a Loquacious Handler!
overLoq :: (Foldable f, Loquacious s) => f (LoqHandler (Doc Ann)) -> s -> Doc Ann
overLoq hs x = loqHandles (Loq x) (loq (overLoq hs) x) hs

-- | Recursively encode a 'Functor' instance to 'Loq'.
--
-- Useful for types of kind '* -> *'.
toLoqFunctor :: (Loquacious (f Loq), Functor f, Loquacious a) => f a -> Loq
toLoqFunctor = Loq . fmap toLoq

-- | Decode a recursively encoded 'Loq' via a 'Traversable' instance.
--
-- Useful for types of kind '* -> *'.
fromLoqTraversable :: (Traversable t, Loquacious a, Typeable t) => Loq -> Maybe (t a)
fromLoqTraversable (Loq sm) = cast sm >>= traverse fromLoq

-- | Recursively encode a 'Bifunctor' instance to 'Loq'.
--
-- Useful for types of kind '* -> * -> *'.
toLoqBifunctor :: (Loquacious (f Loq Loq), Bifunctor f, Loquacious a, Loquacious b) => f a b -> Loq
toLoqBifunctor = Loq . bimap toLoq toLoq

-- | Decode a recursively encoded 'Loq' via a 'Traversable' instance.
--
-- Useful for types of kind '* -> *'.
fromLoqBitraversable :: (Bitraversable t, Loquacious a, Loquacious b, Typeable t) => Loq -> Maybe (t a b)
fromLoqBitraversable (Loq sm) = cast sm >>= bitraverse fromLoq fromLoq

-- | Recursively encode a 'Bifunctor' instance to 'Loq', but only on the first type argument.
--
-- Useful for types of kind '* -> k -> *', for example 'Const'.
toLoqFirst :: (Loquacious (f Loq b), Bifunctor f, Loquacious a) => f a b -> Loq
toLoqFirst = Loq . first toLoq

-- | Decode a recursively encoded 'Loq' via a 'Traversable' instance.
--
-- Useful for types of kind '* -> *'.
fromLoqFirst :: (Bitraversable t, Loquacious a, Typeable t, Typeable b) => Loq -> Maybe (t a b)
fromLoqFirst (Loq sm) = cast sm >>= bitraverse fromLoq pure

-- basic base types
instance Loquacious ()
instance Loquacious Bool
instance Loquacious Version where loq _ = pretty . showVersion
instance Loquacious Void

-- basic number types
instance Loquacious Integer
instance Loquacious Natural

instance Loquacious Int
instance Loquacious Int8
instance Loquacious Int16
instance Loquacious Int32
instance Loquacious Int64

instance Loquacious Word
instance Loquacious Word8
instance Loquacious Word16
instance Loquacious Word32
instance Loquacious Word64

instance Loquacious Double
instance Loquacious Float

-- polymorphic types
instance Loquacious a => Loquacious (Identity a) where
  loq m = m . runIdentity
  toLoq = toLoqFunctor
  fromLoq = fromLoqTraversable

instance (Loquacious a, Typeable b) => Loquacious (Const a b) where
  loq m = m . getConst
  toLoq = toLoqFirst
  fromLoq = fromLoqFirst

instance Loquacious a => Loquacious (Maybe a) where
  loq _ Nothing  = mempty
  loq m (Just x) = m x
  toLoq = toLoqFunctor
  fromLoq = fromLoqTraversable

instance Loquacious a => Loquacious (NonEmpty a) where
  loq m = align . list . fmap m . toList
  toLoq = toLoqFunctor
  fromLoq = fromLoqTraversable

instance Loquacious a => Loquacious [a] where
  loq m = align . list . fmap m
  toLoq = toLoqFunctor
  fromLoq = fromLoqTraversable

-- | The first paramater of 'Array' cannot be matched generically.
instance (Ix i, Typeable i, Loquacious a) => Loquacious (Array i a) where
  loq m = align . list . fmap m . toList
  toLoq = toLoqFunctor
  fromLoq = fromLoqTraversable

-- | 'UArray' cannot be matched generically due to boxing requirements.
instance (Ix i, Typeable i, IArray UArray a, Loquacious a) => Loquacious (UArray i a) where
  loq m = align . list . fmap m . elems

instance (Loquacious a, Loquacious b) => Loquacious (Either a b) where
  loq m (Left x)  = m x
  loq m (Right x) = m x
  toLoq = toLoqBifunctor
  fromLoq = fromLoqBitraversable

instance (Loquacious a, Loquacious b) => Loquacious (a, b) where
  loq m (a,b) = align $ tupled [m a, m b]
  toLoq = toLoqBifunctor
  fromLoq = fromLoqBitraversable

-- | FIXME The first paramater of cannot be matched generically, no @Tritraversable@.
instance (Loquacious a, Loquacious b, Loquacious c) => Loquacious (a, b, c) where
  loq m (a,b,c) = align $ tupled [m a, m b, m c]
  toLoq = toLoqBifunctor
  fromLoq = fromLoqBitraversable

instance Loquacious t => Loquacious (Complex t) where
  loq m (a :+ b) = m a <+> "+" <+> m b <> "i"
  toLoq = toLoqFunctor
  fromLoq = fromLoqTraversable

-- | FIXME Cannot match generic 'Ratio's due to lack of 'Functor' instance.
instance Loquacious t => Loquacious (Ratio t) where
  loq m x = m (numerator x) <> "/" <> m (denominator x)
  -- toLoq = toLoqFunctor
  -- fromLoq = fromLoqTraversable

-- basic string types
instance Loquacious Char
instance {-# OVERLAPS #-} Loquacious String
instance Loquacious Text.Text
instance Loquacious LazyText.Text
-- TODO instance Loquacious ByteString.ByteString
-- TODO instance Loquacious LazyByteString.ByteString


-- Call Stack Support
instance Loquacious CallStack where
  loq m = withFrozenCallStack ( align . vsep . fmap messageStack . getCallStack )
    where messageStack (func, srcLoc) = fromString func <+> "←" <+> m srcLoc

instance Loquacious SrcLoc where
  loq m SrcLoc{..} = msgFile <+> parens msgPackage
    where
      msgFile = fromString srcLocFile <> colon <> m srcLocStartLine <> colon <> m srcLocStartCol
      msgPackage = fromString srcLocPackage <> colon <> fromString srcLocModule

-- Proxy and other Type Representations
instance Loquacious TypeRep where loq _ = viaShow

instance (Typeable a) => Loquacious (Proxy a) where
  loq m _ = m $ typeRep (Proxy @a)

instance (Typeable a, Typeable b) => Loquacious (a -> b) where
  loq m = m . typeOf

instance (Typeable a, Typeable b) => Loquacious (a :~: b) where
  loq m _ = m (typeRep (Proxy @a)) <+> ":~:" <+> m (typeRep (Proxy @b))

instance (Typeable a, Typeable b) => Loquacious (a :~~: b) where
  loq m _ = m (typeRep (Proxy @a)) <+> ":~~:" <+> m (typeRep (Proxy @b))

-- Exception Types
instance Loquacious E.AllocationLimitExceeded where loq _ = viaShow
instance Loquacious E.ArithException where loq _ = viaShow
instance Loquacious E.ArrayException where loq _ = viaShow
instance Loquacious E.AssertionFailed where loq _ = viaShow
instance Loquacious E.AsyncException where loq _ = viaShow
instance Loquacious E.BlockedIndefinitelyOnMVar where loq _ = viaShow
instance Loquacious E.BlockedIndefinitelyOnSTM where loq _ = viaShow
instance Loquacious E.CompactionFailed where loq _ = viaShow
instance Loquacious E.Deadlock where loq _ = viaShow
instance Loquacious E.IOException where loq _ = viaShow
instance Loquacious E.NestedAtomically where loq _ = viaShow
instance Loquacious E.NoMethodError where loq _ = viaShow
instance Loquacious E.NonTermination where loq _ = viaShow
instance Loquacious E.PatternMatchFail where loq _ = viaShow
instance Loquacious E.RecConError where loq _ = viaShow
instance Loquacious E.RecSelError where loq _ = viaShow
instance Loquacious E.RecUpdError where loq _ = viaShow
instance Loquacious E.SomeAsyncException where loq _ = viaShow
instance Loquacious E.SomeException where loq _ = viaShow
instance Loquacious E.TypeError where loq _ = viaShow
instance Loquacious EB.FixIOException where loq _ = viaShow
