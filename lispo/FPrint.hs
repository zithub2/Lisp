module FPrint
( show1, show3
, show1Ln, show3Ln
, print1, print3
) where

import qualified L
import F (F(N,C))

--show1 (N n) = show n --debug
show1 (N n) = n
show1 (C x y) = concat [[L.lg], show1 x, showy y, [L.rg]] where
  showy (C a b) = concat [L.sep, show1 a, showy b]
  showy n@(N name)
    | name==L.坤 = ""
    | otherwise = L.sep3 ++ show1 n
  showLast l = L.sep3 ++  show1 l

show3 (C x y) = show1 x : show3 y
show3 _ = []

show1Ln = (++"\n") . show1
show3Ln = concatMap (++"\n") . show3

print1 = putStr . show1Ln
print3 = putStr . show3Ln

