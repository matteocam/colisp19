module Colisp19 where

import Data.Char
import Text.Parsec
import qualified Data.Map as Map

-- Parsing

data TExp = 
    ListE [TExp] |
    AtomE TAtom |
    QuoteE TExp 
    deriving (Eq, Show)

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

-- This is a context stack
type Ctx = [Map.Map String Val]

type Output = Val -- Should be (Ctx, Val later)

data Val = 
    VNum Int |
    VStr String |
    VSym String |
    VFun FunDef |
    VQuoted TExp

instance Eq Val where
    VFun _ == _ = error "Comparing functions"
    _ == VFun _ = error "Comparing functions"
    VNum x == VNum y      = x == y
    VStr x == VStr y      = x == y
    VSym x == VSym y      = x == y
    VQuoted x == VQuoted y = x == y
    _ == _                 = False


-- TODO: name resolution

-- XXX: For side effects, evaluation should also return a new ctx
eval_impl :: Ctx -> TExp -> Output
eval_impl ctx (AtomE (AId id)) = eval_id ctx id
eval_impl _ (AtomE x) = eval_atom x
eval_impl _ (QuoteE x) = VQuoted x
eval_impl ctx (ListE (fn_e:rst)) = eval_fun ctx fn (map (eval_impl ctx) rst)
    where (VFun fn) = eval_impl ctx fn_e
eval_impl ctx (ListE []) = error "Empty program"

default_ctx :: Ctx
default_ctx = builtins_ctx

eval :: TExp -> Output
eval = eval_impl default_ctx 

eval_fun :: Ctx -> FunDef -> [Val] -> Output
eval_fun ctx fn params = eval_fun' ctx (fimpl f) where
    VFun f = retrieve ctx (fname fn)
    eval_fun' ctx (FExp fexp) = error "Still working on it..." -- TODO
    eval_fun' ctx (FNative fnat) = fnat params -- XXX: Should also change ctx in some way


eval_id :: Ctx -> String -> Val
eval_id = retrieve 

retrieve :: Ctx -> String -> Val
retrieve (c:cs) id = case Map.lookup id c of
    Just v -> v
    Nothing -> retrieve cs id
retrieve [] id = error $ "Cannot find id " ++ id

eval_atom :: TAtom -> Output
eval_atom (ANumber n) = VNum n
eval_atom (ASymbol s) = VSym $ ":" ++ s
eval_atom (AString s) = VStr s

-- builtins
data FunImpl = FExp TExp | FNative ([Val] -> Val)

data FunDef = FunDef { 
    fname :: String,
    fparams :: [String],
    fimpl :: FunImpl
    } 

builtins = [eq_fn, if_fn, sum_fn]
builtins_ctx = [m] where
    m = Map.fromList [(fname f, VFun f) | f <- builtins]

eq_fn = FunDef { 
    fname = "eq",
    fparams = ["a", "b"],
    fimpl = FNative $ \[a,b] -> if a == b then VNum 1 else VNum 0
}

if_fn = FunDef {
    fname = "if",
    fparams = ["t", "a", "b"],
    fimpl = FNative $ \[VNum t,a,b] -> if t /= 0 then a else b   
}

sum_fn = FunDef {
    fname = "+",
    fparams = ["a", "b"],
    fimpl = FNative $ \[VNum a, VNum b] -> VNum (a + b)
}

