module ℤ where

-- | We need some inductive whole numbers.
data ℤ = P ℤ | Z | S ℤ

-- | Since there are many ways to build a given number, we should like to be
-- able to find the canonical way of building.
--
-- It is not entirely trivial. We must repeat normalization passes until we are
-- sure that all irregularities are smoothed out. But we also must stop some
-- time and not enter an endless cycle.
type family Normalize (x ∷ ℤ) where
  Normalize (P (S x)) = Normalize x
  Normalize (S (P x)) = Normalize x
  Normalize (P x) = Normalize' (IsNormal (P (Normalize x))) (P (Normalize x))
  Normalize (S x) = Normalize' (IsNormal (S (Normalize x))) (S (Normalize x))
  Normalize x = x

type family Normalize' (isNormal ∷ Bool) (x ∷ ℤ) ∷ ℤ where
  Normalize' False x = Normalize x
  Normalize' True x = x

data Direction = Descending | Ascending

type family IsNormal (x ∷ ℤ) ∷ Bool where
  IsNormal (P x) = IsMonotone Descending x
  IsNormal Z = True
  IsNormal (S x) = IsMonotone Ascending x

type family IsMonotone (direction ∷ Direction) (x ∷ ℤ) ∷ Bool where
  IsMonotone _ Z = True
  IsMonotone Ascending (S x) = IsMonotone Ascending x
  IsMonotone Ascending (P x) = False
  IsMonotone Descending (S x) = False
  IsMonotone Descending (P x) = IsMonotone Descending x

-- | The obvious additive inverse. We should like to have
-- `Normalize ∘ Invert = Invert ∘ Normalize`.
type family Invert (x ∷ ℤ) where
  Invert (P x) = S (Invert x)
  Invert Z = Z
  Invert (S x) = P (Invert x)

-- | The obvious addition.
type family Add (x ∷ ℤ) (y ∷ ℤ) where
  Add (S x) y = Add x (S y)
  Add Z y = Normalize y
  Add (P x) y = Add x (P y)

-- | A shady multiplication. It defers the work to a specialized fold with an
-- accumulator set to zero.
type family Multiply (x ∷ ℤ) (y ∷ ℤ) where
  Multiply x y = Multiply' x y Z

-- | The actual multiplication function. _«Add `y` to `accumulator` `x`
-- times.»_ We expect it to obey the ring laws when initial `accumulator` is
-- zero.
type family Multiply' (x ∷ ℤ) (y ∷ ℤ) (accumulator ∷ ℤ) where
  Multiply' Z y accumulator = accumulator
  Multiply' (S x) y accumulator = Multiply' x y (Add y accumulator)
  Multiply' (P x) y accumulator = Multiply' x y (Add (Invert y) accumulator)
