{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

module SimpleGrad (grad)
where

import Prelude.Unicode

class Grad a where
  grad ∷ Double → (a → Double) → a → a

instance Grad Double where
  grad ε f x = (f (x+ε) - f x) / ε

instance Grad a ⇒ Grad [a] where
  grad ε f xs =
    [grad ε (f_ i) x_i | (x_i,i) ← zip xs [0..]] where
        f_ i x_i = f $ take i xs ++ [x_i] ++ drop (i+1) xs

instance (Grad a, Grad b) ⇒ Grad (a,b) where
  grad ε f (x,y) = (grad ε (f ∘ (,y)) x, grad ε (f ∘ (x,)) y)

instance Grad a ⇒ Grad (Maybe a) where
  grad ε f x = x >>= Just ∘ grad ε (f ∘ Just)
