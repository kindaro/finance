module Finance where

import Prelude.Unicode ((≤), (∧), (≡))
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

-- | Continuous interest compounding with an additional steady income.
-- Δ$ = r$Δt + αΔt.
continuousWithLinearIncome ∷ Rate → Time → (Money ÷ Time) → Money → Money
continuousWithLinearIncome r t α x = case r of
  0 → x  +  α × t
  r' → e°(r' × t) × x  +  α ÷ r'

samplingRate ∷ Rate
samplingRate = 1

discountedPriceToSpotRate ∷ Time → Money → Rate
discountedPriceToSpotRate maturity price = (reciprocal price × (1 ∷ Money ÷ Time)) ° (reciprocal maturity × (1 ∷ Time))  -  1

spotRateToDiscountedPrice ∷ Time → Rate → Money
spotRateToDiscountedPrice maturity rate = reciprocal ((1  +  rate)°(maturity × samplingRate)) × (1 ∷ Money ÷ Time)
