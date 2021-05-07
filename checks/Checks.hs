{-# options_ghc -Wno-orphans #-}

module Main where

import Prelude
import Prelude.Unicode ((≡), (≤), (≥), (∘))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Tagged
import Data.Proxy

import Finance
import qualified ℤ
import ℤ (ℤ (..))
import TypedArithmetic

main ∷ IO ( )
main = defaultMain checks

checks ∷ TestTree
checks = testGroup ""
  [ testProperty "discrete < continuous"
    \ (NonNegative interest) (NonNegative time) wealth
    → abs (discrete interest (truncate time) wealth) ≤ abs (continuous interest time wealth)
  , testGroup "identity"
    [ testProperty "discrete 0 = id" \ time → discrete 0 time ↔ id @Money
    , testProperty "discrete > id"
      \ (Positive interest) (Positive time) (Positive wealth)
      → discrete interest time wealth ≥ wealth
    , testProperty "continuous 0 = id" \ time → continuous 0 time ↔ id @Money
    , testProperty "continuous > id"
      \ (Positive interest) (Positive time) (Positive wealth)
      → continuous interest time wealth ≥ wealth
    , testProperty "continuous = continuous with linear income 0"
      $ continuous ↔ \ interest time → continuousWithLinearIncome interest time 0
    , testProperty "continuous 0 with linear income = id"
      \ wealth → continuousWithLinearIncome 0 1 ((1 ∷ Rate) × wealth) 0 ↔ wealth
    , testGroup "rates and discounts"
      [ testProperty "rate to price and back"
        \ maturity (NonNegative rate') →
          let rate = (rate' - fromIntegral (truncate rate')) / 10
          in rate ~~~ (discountedPriceToSpotRate maturity ∘ spotRateToDiscountedPrice maturity) rate
      , testProperty "price to rate and back"
        \ (Positive maturity) (Positive price') →
          let price = (price' - fromIntegral (truncate price'))
          in price ~~~ (spotRateToDiscountedPrice maturity ∘ discountedPriceToSpotRate maturity) price
      ]
    ]
  , testGroup "cases"
    [ testCase "discrete" do assertEqual "" 2593.7427 (discrete 0.1 10 1000)
    , testCase "continuous" do assertEqual "" 2718.2817 (continuous 0.1 10 1000)
    , testCase "ℤ normalization" do assertEqual "" (Proxy @(ℤ.Add (S(S Z)) (P(P Z)))) (Proxy @Z)
    ]
  ]

x ~~~ y = counterexample (show (x, y)) (abs (x - y) < 0.00001)

class ExtensionalEquality α where
  isExtensionallyEqual ∷ α → α → Property

instance {-# overlappable #-} (Eq α, Show α, Arbitrary α) ⇒ ExtensionalEquality α where
  isExtensionallyEqual x y = property $ x === y

instance (ExtensionalEquality β, Show α, Arbitrary α) ⇒ ExtensionalEquality (α → β) where
  isExtensionallyEqual f g = property \ x → f x ↔ g x

infix 4 ↔
(↔) ∷ ExtensionalEquality α ⇒ α → α → Property
(↔) = isExtensionallyEqual

instance Arbitrary β ⇒ Arbitrary (Tagged α β) where
  arbitrary = fmap Tagged arbitrary
