module Finance where

import Prelude.Unicode ((≤), (∧), (≡), (∘))
import Data.Function
import Data.Map qualified as Map

import TypedArithmetic

type key ⇸ value = Map.Map key value

-- | This is the constant _e_.
e ∷ Dimensionless
e = exp 1

-- | Discrete interest compounding.
discrete ∷ Dimensionless → DiscreteTime → Money → Money
discrete interest = fix \ f t x → if t ≤ 0 then x else f (t - 1) (x × (interest + 1))

-- | Continuous interest compounding.
continuous ∷ Dimensionless → Time → Money → Money
continuous r t x = (e°(r × t × samplingRate)) × x

-- | Continuous interest compounding with an additional steady income.
-- Δ$ = r$Δt + αΔt.
continuousWithLinearIncome ∷ Dimensionless → Time → (Money ÷ Time) → Money → Money
continuousWithLinearIncome r t α x = case r of
  0 → x  +  α × t
  r' → e°(r' × t × samplingRate) × x  +  α ÷ (r' × samplingRate)

samplingRate ∷ Rate
samplingRate = 1

discountedPriceToSpotRate ∷ Time → Money → Rate
discountedPriceToSpotRate maturity price = (reciprocal price × (1 ∷ Money ÷ Time)) ° (reciprocal maturity × (1 ∷ Time))  -  1

spotRateToDiscountedPrice ∷ Time → Rate → Money
spotRateToDiscountedPrice maturity rate = reciprocal ((1  +  rate)°(maturity × samplingRate)) × (1 ∷ Money ÷ Time)

spotRatesToForwardRates ∷ Time ⇸ Rate → Time ⇸ Rate
spotRatesToForwardRates = snd ∘ Map.mapAccumWithKey f (0 ∷ Time, 0 ∷ Rate)
  where
    f ∷ (Time, Rate) → Time → Rate → ((Time, Rate), Rate)
    f (previousTime, previousRate) time rate =
      let forwardRate = ((rate  +  1)°(time × samplingRate) ÷ (previousRate  +  1)°(previousTime × samplingRate) × (1 ∷ Rate)) ° reciprocal ((time  -  previousTime) × samplingRate)  -  1
      in ((time, rate), forwardRate)

forwardRatesToSpotRates ∷ Time ⇸ Rate → Time → Money
forwardRatesToSpotRates forwardRates = _u
