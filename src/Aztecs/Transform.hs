{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.Transform
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Transform
  ( -- * Components

    -- ** 2D
    Transform2D,
    transform2d,
    GlobalTransform2D,
    Size2D,

    -- ** Generic
    Transform (..),
    GlobalTransform (..),
    transform,
    Size (..),
    module Linear,
  )
where

import Aztecs.ECS
import Aztecs.Hierarchy
import Control.Monad
import Data.Data (Typeable)
import qualified Data.Set as Set
import Linear
import Prelude hiding (lookup)

type GlobalTransform2D = GlobalTransform (V2 Int) (V2 Float) Float

newtype GlobalTransform t s r = GlobalTransform {unGlobalTransform :: Transform t s r}
  deriving (Show)

instance (Monad m, Typeable t, Typeable s, Typeable r) => Component m (GlobalTransform t s r)

type Size2D = Size (V2 Float)

newtype Size a = Size {unSize :: a}
  deriving (Show)

instance (Monad m, Typeable a) => Component m (Size a)

type Transform2D = Transform (V2 Int) (V2 Float) Float

data Transform t s r = Transform
  { transformTranslation :: !t,
    transformScale :: !s,
    transformRotation :: !r
  }
  deriving (Show)

instance (Num t, Num s, Num r) => Semigroup (Transform t s r) where
  Transform t1 s1 r1 <> Transform t2 s2 r2 = Transform (t1 + t2) (s1 * s2) (r1 + r2)

instance (Num t, Num s, Num r) => Monoid (Transform t s r) where
  mempty = Transform 0 1 0

instance
  (Monad m, Typeable t, Typeable s, Typeable r, Num t, Num s, Num r) =>
  Component m (Transform t s r)
  where
  componentOnInsert e localT = do
    globalT <- computeGlobalTransform e localT
    insertUntracked e . bundle $ GlobalTransform @t @s @r globalT
    propagateToChildren e globalT

  componentOnChange e _ newLocalT = do
    !globalT <- computeGlobalTransform e newLocalT
    insertUntracked e . bundle $ GlobalTransform @t @s @r globalT
    propagateToChildren e globalT

  componentOnRemove e _ = void $ remove @_ @(GlobalTransform t s r) e

-- | Propagate a global transform to all children of an entity.
propagateToChildren ::
  forall m t s r.
  (Monad m, Typeable t, Typeable s, Typeable r, Num t, Num s, Num r) =>
  EntityID ->
  Transform t s r ->
  Access m ()
propagateToChildren parentE parentGlobal = do
  maybeChildren <- lookup @_ @Children parentE
  case maybeChildren of
    Nothing -> return ()
    Just children -> mapM_ propagateToChild (Set.toList $ unChildren children)
  where
    propagateToChild :: EntityID -> Access m ()
    propagateToChild childE = do
      !maybeLocalT <- lookup @_ @(Transform t s r) childE
      case maybeLocalT of
        Nothing -> return ()
        Just localT -> do
          let !childGlobal = localT <> parentGlobal
          insertUntracked childE . bundle $ GlobalTransform @t @s @r childGlobal
          propagateToChildren childE childGlobal

-- | Compute the global transform for an entity based on its local transform and parent.
computeGlobalTransform ::
  forall m t s r.
  (Monad m, Typeable t, Typeable s, Typeable r, Num t, Num s, Num r) =>
  EntityID ->
  Transform t s r ->
  Access m (Transform t s r)
computeGlobalTransform e localT = do
  maybeParent <- lookup @_ @Parent e
  case maybeParent of
    Nothing -> return localT
    Just (Parent parentE) -> do
      maybeParentGlobal <- lookup @_ @(GlobalTransform t s r) parentE
      return $ localT <> maybe mempty unGlobalTransform maybeParentGlobal

transform2d :: Transform2D
transform2d = transform

transform :: (Num t, Num s, Num r) => Transform t s r
transform = mempty
