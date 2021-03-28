module TypedArithmetic where

import Data.Tagged

import qualified ℤ
import ℤ (ℤ (..))
import TypeFunctions

type Add = ZipWith ℤ.Add
type Invert = Map ℤ.Invert

type DimensionNone = '[Z, Z]
type DimensionTime = '[S Z, Z]
type DimensionMoney = '[Z, S Z]

type Dimensionless = Tagged DimensionNone Float
type Money = Tagged DimensionMoney Float
type Time = Tagged DimensionTime Float
type DiscreteTime = Tagged DimensionTime Int
type Rate = Tagged (Invert DimensionTime) Float

type family x × y where
  Tagged d₁ α × Tagged d₂ α = Tagged (Add d₁ d₂) α

type family x ÷ y where
  Tagged d₁ α ÷ Tagged d₂ α = Tagged (Add d₁ (Invert d₂)) α

infixl 7 ×
(×) ∷ Prelude.Num α ⇒ Tagged x α → Tagged y α → Tagged (Add x y) α
Tagged x × Tagged y = Tagged (x Prelude.* y)

infixl 7 ÷
(÷) ∷ Prelude.Fractional α ⇒ Tagged x α → Tagged y α → Tagged (Add x (Invert y)) α
Tagged x ÷ Tagged y = Tagged (x Prelude./ y)

infixr 8 °
(°) ∷ Prelude.Floating α ⇒ Tagged x α → Tagged DimensionNone α → Tagged x α
Tagged x ° Tagged y = Tagged (x ** y)
