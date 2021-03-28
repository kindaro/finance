module Finance where

import Prelude.Unicode ((≤))
import Data.Function

import TypedArithmetic

-- | This is the constant _e_.
e ∷ Dimensionless
e = exp 1

-- | Discrete interest compounding.
discrete ∷ Rate → DiscreteTime → Money → Money
discrete interest = fix \ f t x → if t ≤ 0 then x else f (t - 1) (x × (interest + 1) × (1 ∷ Time))

-- | Continuous interest compounding.
continuous ∷ Rate → Time → Money → Money
continuous r t x = (e°(r × t)) × x
