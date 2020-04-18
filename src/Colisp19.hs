module Colisp19 where

data TExp = 
    ListE [TExp] |
    AtomE TAtom |
    QuoteE TExp 

data TAtom = 
    TNumber Int |
    TSymbol String |
    TId String



greet :: String -> String
greet who =
  "Hello, " <> who <> "!"