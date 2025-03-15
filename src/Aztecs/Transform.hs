{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.SDL
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

    -- * Systems

    -- ** 2D
    propagateTransforms2D,

    -- ** Generic
    propagateTransforms,
    propagateTransformHierarchies,
    propagateTransformHierarchy,
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import Aztecs.Hierarchy
import Data.Data (Typeable)
import Linear

type Transform2D = Transform (V2 Int) Float (V2 Float)

data Transform t s r = Transform
  { transformTranslation :: t,
    transformScale :: s,
    transformRotation :: r
  }
  deriving (Show)

instance (Num t, Num s, Num r) => Semigroup (Transform t s r) where
  Transform t1 s1 r1 <> Transform t2 s2 r2 = Transform (t1 + t2) (s1 * s2) (r1 + r2)

instance (Num t, Num s, Num r) => Monoid (Transform t s r) where
  mempty = Transform 0 1 0

transform2d :: V2 Int -> Float -> V2 Float -> Transform2D
transform2d = Transform

transform :: (Num t, Num s, Num r) => Transform t s r
transform = mempty

instance (Typeable t, Typeable s, Typeable r) => Component (Transform t s r)

type GlobalTransform2D = GlobalTransform (V2 Int) Float (V2 Float)

newtype GlobalTransform t s r = GlobalTransform {unGlobalTransform :: Transform t s r}

instance (Typeable t, Typeable s, Typeable r) => Component (GlobalTransform t s r)

type Size2D = Size (V2 Float)

newtype Size a = Size {unSize :: a}

instance (Typeable a) => Component (Size a)

propagateTransforms2D :: System (Access ())
propagateTransforms2D = propagateTransforms @(V2 Int) @Float @(V2 Float)

propagateTransforms ::
  forall t s r.
  (Typeable t, Typeable s, Typeable r, Num t, Num s, Num r) =>
  System (Access ())
propagateTransforms =
  mapM_
    (\(e, a) -> A.insert e . bundle $ GlobalTransform @t @s @r a)
    . concatMap toList
    <$> propagateTransformHierarchies

-- | Propagate all hierarchies of transform components.
--
-- @since 0.9
propagateTransformHierarchies ::
  (Typeable t, Typeable s, Typeable r, Num t, Num s, Num r) =>
  System [Hierarchy (Transform t s r)]
propagateTransformHierarchies = do
  hs <- hierarchies fetch
  return $ map propagateTransformHierarchy hs

propagateTransformHierarchy ::
  (Num t, Num s, Num r) => Hierarchy (Transform t s r) -> Hierarchy (Transform t s r)
propagateTransformHierarchy = mapWithAccum (\_ t acc -> let t' = t <> acc in (t', t')) mempty
