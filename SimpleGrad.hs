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

gradConstruct0 ∷ Double → (z → Double) → z → z
gradConstruct0 _ε _f constructor =
  constructor

gradConstruct1 ∷ Grad a ⇒ Double → (z → Double) → (a → z) → a → z
gradConstruct1 ε f constructor x =
  constructor dx where
    dx = grad ε (f ∘ constructor) x

gradConstruct2 ∷ (Grad a, Grad b) ⇒ Double → (z → Double) → (a → b → z) → a → b → z
gradConstruct2 ε f constructor x y =
  constructor dx dy where
    dx = grad ε (f ∘ \x' → constructor x' y ) x
    dy = grad ε (f ∘ \y' → constructor x  y') y

instance Grad a ⇒ Grad [a] where
  grad ε f []     = gradConstruct0 ε f []
  grad ε f (x:xs) = gradConstruct2 ε f (:) x xs

instance (Grad a, Grad b) ⇒ Grad (a,b) where
  grad ε f (x,y) = gradConstruct2 ε f (,) x y

instance Grad a ⇒ Grad (Maybe a) where
  grad ε f Nothing  = gradConstruct0 ε f Nothing
  grad ε f (Just x) = gradConstruct1 ε f Just x

instance (Grad a, Grad b) ⇒ Grad (Either a b) where
  grad ε f (Left x)  = gradConstruct1 ε f Left x
  grad ε f (Right x) = gradConstruct1 ε f Right x
