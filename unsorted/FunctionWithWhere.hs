-- FunctionWithWhere.hs

module FunctionWithWhere where

multi1 = x * 3 + y
 where x = 3
       y = 1000
multi2 = x * 5 
 where y = 10 
       x = 10 * 5 + y

waxOn  = x * 5 
 where z = 7 
       y = z + 8
       x = y ^ 2

triple x = x * 3

waxOff x = triple x
