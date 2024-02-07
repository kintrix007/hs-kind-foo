{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableSuperClasses #-}

import GHC.Exts (Constraint)

class FooBarBaz f where
  type family Allowed (f :: * -> *) :: * -> Constraint

  takeOut :: Allowed f a => f a -> a

class (k a :: Constraint, l a :: Constraint) => And k l a
instance (k a, l a) => And k l a

-- NOTE: Although in cases when it is fully applied, it is identical.
-- However, it cannot be partially applied
-- type And (k :: * -> Constraint) (l :: * -> Constraint) (a :: *) = (k a, l a)

foo :: (And Eq Num a) => a -> a
-- foo :: (Eq a, Num a) => a -> a
foo x =
  if x * 5 == 10
    then abs x
    else x

instance FooBarBaz Maybe where
  type Allowed Maybe = And Eq Bounded

  takeOut Nothing = error "Oof"
  takeOut (Just x) =
    if x == minBound
      then maxBound
      else x

class NoConstraint a
instance NoConstraint a

main = do
  print $ takeOut (Just LT)
  print $ takeOut (Just EQ)
  print $ takeOut (Just GT)
