--lispo: lisp only
module Main(main) where

import FRead
import FEval
import FPrint

--main = interact rp -- debug
main = interact resLn

--read-eval-show/print
rp = show3Ln . readIndent
resLn = show3Ln . veval3 . readIndent

res1Ln = show1Ln . veval1 . fst . read1
res3Ln = show3Ln . veval3 . fst . read3
rep1 = putStr . res1Ln
rep3 = putStr . res3Ln

