{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Text.Loquacious.Annotations
Description : Generic Annotations for Pretty Printing
Stability   : experimental

An Annotation Type Hierarchy, much like 'Control.Exception.Exception' and
'Control.Exception.SomeException'.

The general idea is that we can annotate a 'Data.Text.Prettyprint.Doc.Doc' with
any 'Annotation' instance, and then the renderer can use 'fromAnn' and
'Data.Text.Prettyprint.Doc.alterAnnotationsS' to extract the annotations it
cares about.
-}

module Text.Loquacious.Annotations
  ( -- * Existential Annotations
    Annotation(..)
  , Ann(..)
  ) where

import           Data.Typeable (Typeable, cast)

-- | Encapsulate an 'Annotation', allowing us to mix types of annotations while
-- building up a tree, compare with 'Control.Exception.SomeException'.
data Ann = forall a. Annotation a => Ann a

-- | The typeclass for annotations, analogous to 'Control.Exception.Exception'.
class Typeable a => Annotation a where
  -- | Embed some annotation in an existential type.
  toAnn :: a -> Ann
  toAnn = Ann
  -- | Attempt to extract some annotation from an existential type.
  fromAnn :: Ann -> Maybe a
  fromAnn (Ann x) = cast x

instance Annotation Ann where
  toAnn a = a
  fromAnn = Just
