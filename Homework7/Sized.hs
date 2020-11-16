{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Sized where

import Data.Monoid

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd

instance Monoid Size where
  mempty  = Size 0
  mappend = (+)

-- Added because https://stackoverflow.com/questions/53622428/a-basic-monoid-definition-gives-no-instance-for-semigroup-mymonoid-arising-fr
instance Semigroup Size where
  x <> y = x `mappend` y
