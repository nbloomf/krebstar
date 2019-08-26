module Ned.Debug where

import Data.Proxy

import Ned.Data

type Nat30 = Nat (Doub (Incr (Doub (Incr (Doub (Incr (Doub (Incr Zero))))))))

type Nat8 = Nat (Doub (Doub (Doub (Incr Zero))))

