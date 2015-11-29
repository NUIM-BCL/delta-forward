{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

module SimpleGrad (grad)
where

import Prelude.Unicode

class Grad a where
  grad ∷ Double → (a → Double) → a → a

instance Grad Double where
  -- Asymmetric Difference:
  -- grad ε f x = (f (x+ε) - f x) / ε         
  -- Symmetric Difference:
  grad ε f x = (f (x+ε) - f (x-ε)) / (2⋅ε)

instance Grad a ⇒ Grad [a] where
  grad ε f xs =
    [grad ε (f_ i) x_i | (x_i,i) ← zip xs [0..]] where
      f_ i x_i = f $ xs1 ++ [x_i] ++ xs2 where
        (xs1, (_:xs2)) = splitAt i xs

instance (Grad a, Grad b) ⇒ Grad (a,b) where
  grad ε f (x,y) = (grad ε (f ∘ (,y)) x, grad ε (f ∘ (x,)) y)

instance Grad a ⇒ Grad (Maybe a) where
  grad ε f x = x >>= Just ∘ grad ε (f ∘ Just)
