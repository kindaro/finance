{-# options_ghc -Wno-orphans #-}

module Main where

import Prelude
import Prelude.Unicode ((≡), (≤), (≥))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Tagged
import Data.Proxy

import Finance

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
    ]
  , testGroup "cases"
    [ testCase "discrete" do assertEqual "" 2593.7427 (discrete 0.1 10 1000)
    , testCase "continuous" do assertEqual "" 2718.2817 (continuous 0.1 10 1000)
    , testCase "ℤ normalization" do assertEqual "" (Proxy @(ℤAdd (S(S Z)) (P(P Z)))) (Proxy @Z)
    ]
  ]

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
