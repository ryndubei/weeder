{-# LANGUAGE TypeFamilies #-}
module Spec.TypeFamilies.TypeFamilies where

-- It's hard to find out whether a particular type family instance is used or
-- not, but easy to find out whether the type family itself is used.
-- Therefore type family intstances are marked as implicit roots, but without a
-- dependency on the type family. 

data A

type family Family a

type instance Family Int = A

root :: Family Int
root = undefined

data B

type family UnusedFamily a

type instance UnusedFamily Int = B

-- Data families should behave in the same way

data C

data family DF a

data instance DF Int = DF { getC :: C }

root2 :: DF Int
root2 = undefined

data D

data family UnusedDF a

data instance UnusedDF Int = UDF D
