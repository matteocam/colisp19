module Colisp19 where

import Data.Char
import Text.Parsec
import qualified Data.Map as Map

-- Parsing

data TExp = 
    ListE [TExp] |
    AtomE TAtom |
    QuoteE TExp 
    deriving (Show)

data TAtom = 
    ANumber Int |
    ASymbol String |
    AString String |
    AId String
    deriving (Eq, Ord, Show)

p_exp :: Parsec String st TExp
p_exp = spaces *> p_exp_proper

p_exp_proper = choice [ ListE <$> p_list_e
                , AtomE <$> p_atom
                , QuoteE <$> p_quote_e
                ]
        <?> "Lisp Expression"

p_list_e = between (char '(') (char ')') (many1 (p_exp <* spaces))

p_atom = choice [ ANumber <$> p_num
                , ASymbol <$> (char ':' *> p_str)
                , AString <$> p_str
                , AId <$> p_str_id
                ] 

p_str :: Parsec String st String
p_str = between (char '"') (char '"') (many anyChar)

p_str_id :: Parsec String st String
p_str_id = spaces *> p_str_id'
    where p_str_id' = (:) <$> letter <*> (many (letter <|> char '-'))

p_quote_e :: Parsec String st TExp
p_quote_e = (char '\'' *> p_exp_proper)

p_num :: Parsec String st Int
p_num = read <$> (many1 $ oneOf "0123456789")


-- Evaluation
type Ctx = Map.Map String Val

type LispFun = [Val] -> Val

data Val = 
    VNum Int |
    VStr String |
    VSym String |
    VFun LispFun |
    VQuoted TExp
-- TODO: make instances of Eq and such

-- XXX: Shouldn't evaluation return a value and a new context?
eval :: Ctx -> TExp -> Val
eval ctx (AtomE (AId id)) = eval_id ctx id
eval _ (AtomE x) = eval_atom x
eval _ (QuoteE x) = VQuoted x
eval ctx (ListE (fn_e:rst)) = eval_fun ctx fn (map (eval ctx) rst)
    where (VFun fn) = eval ctx fn_e
eval ctx (ListE []) = error "Empty program"

eval_fun :: Ctx -> LispFun -> [Val] -> Val
eval_fun ctx fn params = undefined

eval_id :: Ctx -> String -> Val
eval_id = undefined

eval_atom :: TAtom -> Val
eval_atom (ANumber n) = VNum n
eval_atom (ASymbol s) = VSym $":" ++ s
eval_atom (AString s) = VStr s


