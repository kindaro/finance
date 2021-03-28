module Finance where

import Prelude.Unicode ((≤))
import Data.Tagged
import Data.Function

-- | We need some inductive whole numbers.
data ℤ = P ℤ | Z | S ℤ

-- | Since there are many ways to build a given number, we should like to be
-- able to find the canonical way of building.
--
-- It is not entirely trivial. We must repeat normalization passes until we are
-- sure that all irregularities are smoothed out. But we also must stop some
-- time and not enter an endless cycle.
type family ℤNormalize (x ∷ ℤ) where
  ℤNormalize (P (S x)) = ℤNormalize x
  ℤNormalize (S (P x)) = ℤNormalize x
  ℤNormalize (P x) = ℤNormalize' (IsNormal (P (ℤNormalize x))) (P (ℤNormalize x))
  ℤNormalize (S x) = ℤNormalize' (IsNormal (S (ℤNormalize x))) (S (ℤNormalize x))
  ℤNormalize x = x

type family ℤNormalize' (isNormal ∷ Bool) (x ∷ ℤ) ∷ ℤ where
  ℤNormalize' False x = ℤNormalize x
  ℤNormalize' True x = x

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
-- `ℤNormalize ∘ ℤInvert = ℤInvert ∘ ℤNormalize`.
type family ℤInvert (x ∷ ℤ) where
  ℤInvert (P x) = S (ℤInvert x)
  ℤInvert Z = Z
  ℤInvert (S x) = P (ℤInvert x)

-- | The obvious addition.
type family ℤAdd (x ∷ ℤ) (y ∷ ℤ) where
  ℤAdd (S x) y = ℤAdd x (S y)
  ℤAdd Z y = ℤNormalize y
  ℤAdd (P x) y = ℤAdd x (P y)

-- | A shady multiplication. It defers the work to a specialized fold with an
-- accumulator set to zero.
type family ℤMultiply (x ∷ ℤ) (y ∷ ℤ) where
  ℤMultiply x y = ℤMultiply' x y Z

-- | The actual multiplication function. _«ℤAdd `y` to `accumulator` `x`
-- times.»_ We expect it to obey the ring laws when initial `accumulator` is
-- zero.
type family ℤMultiply' (x ∷ ℤ) (y ∷ ℤ) (accumulator ∷ ℤ) where
  ℤMultiply' Z y accumulator = accumulator
  ℤMultiply' (S x) y accumulator = ℤMultiply' x y (ℤAdd y accumulator)
  ℤMultiply' (P x) y accumulator = ℤMultiply' x y (ℤAdd (ℤInvert y) accumulator)

type family Map (f ∷ k → n) (xs ∷ [k]) ∷ [n] where
  Map _ '[ ] = '[ ]
  Map f (x: xs) = f x: Map f xs

type family ZipWith (f ∷ k → n → m) (xs ∷ [k]) (ys ∷ [n]) ∷ [m] where
  ZipWith _ '[ ] '[ ] = '[ ]
  ZipWith f (x: xs) (y: ys) = f x y: ZipWith f xs ys

type Add = ZipWith ℤAdd
type Invert = Map ℤInvert

type DimensionNone = '[Z, Z]
type DimensionTime = '[S Z, Z]
type DimensionMoney = '[Z, S Z]

type Dimensionless = Tagged DimensionNone Float
type Money = Tagged DimensionMoney Float
type Time = Tagged DimensionTime Float
type DiscreteTime = Tagged DimensionTime Int
type Rate = Tagged (Invert DimensionTime) Float

infixl 7 ×
(×) ∷ Prelude.Num α ⇒ Tagged x α → Tagged y α → Tagged (Add x y) α
Tagged x × Tagged y = Tagged (x Prelude.* y)

infixl 7 ÷
(÷) ∷ Prelude.Fractional α ⇒ Tagged x α → Tagged y α → Tagged (Add x (Invert y)) α
Tagged x ÷ Tagged y = Tagged (x Prelude./ y)

infixr 8 °
(°) ∷ Prelude.Floating α ⇒ Tagged x α → Tagged DimensionNone α → Tagged x α
Tagged x ° Tagged y = Tagged (x ** y)

e ∷ Dimensionless
e = Tagged (exp 1)

discrete ∷ Rate → DiscreteTime → Money → Money
discrete interest = fix \ f t x → if t ≤ 0 then x else f (t - 1) (x × (interest + 1) × (1 ∷ Time))

continuous ∷ Rate → Time → Money → Money
continuous r t x = (e°(r × t)) × x
