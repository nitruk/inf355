module Interlude where

cleave :: a -> [a -> b] -> [b]
cleave x = map ($ x)

spread :: [a -> b] -> [a] -> [b]
spread = zipWith ($)
