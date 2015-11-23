module Data.Predicate 
( et
, vel
, non
, est
, nonEst
, quasi )
where

et :: (a -> Bool) -> (a -> Bool) -> a -> Bool 
p1 `et` p2 = \x -> p1 x && p2 x 

vel :: (a -> Bool) -> (a -> Bool) -> a -> Bool 
p1 `vel` p2 = \x -> p1 x || p2 x 

non :: (a -> Bool) -> a -> Bool 
non p = not . p 

est :: (a -> b) -> (b -> Bool) -> a -> Bool
est f p = p . f

nonEst :: (a -> b) -> (b -> Bool) -> a -> Bool
nonEst f p = not . est f p

quasi :: Eq b => (a -> b) -> b -> a -> Bool
quasi f x = f `est` (== x) 
