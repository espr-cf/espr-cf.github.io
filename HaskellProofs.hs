{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ProofClass2 where

myMaybe = Just 1

maybe_map :: (a -> b) -> Maybe a -> Maybe b
maybe_map f Nothing = Nothing
maybe_map f (Just a) = Just (f a)

data Pair a b = Both a b
type TwoInts = Pair Int Int

two_ints :: TwoInts
two_ints = Both 1 2

{--
data Either a b = Left a | Right b
type Error b = Either String b

either_map :: (a -> b) -> Either c a -> Either c b
either_map f (Left c) = Left c
either_map f (Right a) = Right (f a)

data UnitType = UnitValue
unit_value :: UnitType
unit_value = UnitValue

type EitherMaybe a = Either UnitType a

list_map :: (a -> b) -> List a -> List b
list_map f Nil = Nil
list_map f (Cons first rest) = Cons (f first) (list_map f rest)

data Maybe a = None | Just a
--}

myList = Cons 1 (Cons 2 Nil)

unsafe_head :: List a -> a
unsafe_head (Cons a _) = a

maybe_head :: List a -> Maybe a
maybe_head (Cons a _) = Just a
maybe_head Nil = Nothing

data NonEmptyList a = Cons2 a (List a)

data Z
data S n

type Zero = Z
type One = S Z
--type Two = S One
type Two = S (S Z)


data List a where
  Nil :: List a
  Cons :: a -> List a -> List a


data Vec n a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

safe_head :: Vec (S n) a -> a
safe_head (VCons a _) = a

safe_tail :: Vec (S n) a -> Vec n a
safe_tail (VCons _ v) = v


-- n + m = k
data Plus n m k where
  PlusZ :: Plus Z n n
  PlusS :: Plus n m k -> Plus (S n) m (S k)

one_plus_one_is_two :: Plus One One Two
one_plus_one_is_two = PlusS PlusZ

data Nat n where
  NatZ :: Nat Z
  NatS :: Nat n -> Nat (S n)

lemma1 :: Nat n -> Plus n Z n
lemma1 NatZ = PlusZ
lemma1 (NatS n) = PlusS (lemma1 n)

lemma2 :: Nat n -> Nat m -> Nat k -> Plus n m k -> Plus n (S m) (S k)
lemma2 n m k PlusZ = PlusZ
lemma2 (NatS n) m (NatS k) (PlusS p) = PlusS (lemma2 n m k p)

commutes :: Nat n -> Nat m -> Nat k -> Plus n m k -> Plus m n k
commutes n m k PlusZ = lemma1 k
commutes (NatS n) m (NatS k) (PlusS p) = lemma2 m n k (commutes n m k p)














