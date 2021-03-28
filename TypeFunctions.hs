module TypeFunctions where

type family Map (f ∷ k → n) (xs ∷ [k]) ∷ [n] where
  Map _ '[ ] = '[ ]
  Map f (x: xs) = f x: Map f xs

type family ZipWith (f ∷ k → n → m) (xs ∷ [k]) (ys ∷ [n]) ∷ [m] where
  ZipWith _ '[ ] '[ ] = '[ ]
  ZipWith f (x: xs) (y: ys) = f x y: ZipWith f xs ys
