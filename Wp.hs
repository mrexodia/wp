-- Type primitives
data Primitive = PBool
               | PInt
               deriving Eq

instance Show Primitive where
    show PBool = "bool"
    show PInt = "int"

-- Types (primitive + [primitive]), no array nesting allowed.
data Type = TPrim Primitive
          | TArray Primitive
          deriving Eq

instance Show Type where
    show (TPrim p) = show p
    show (TArray p) = "[" ++ show p ++ "]"

-- Handy shorthand functions
tbool :: Type
tbool = TPrim PBool

tint :: Type
tint = TPrim PInt

tarray :: Type -> Type
tarray (TPrim p) = TArray p
tarray (TArray _) = error "no array nesting allowed"

-- Typed variable
data Var = Var Type String deriving Eq

instance Show Var where
    show (Var t n) = n -- ++ ":" ++ show t

bool :: String -> Var
bool = Var tbool

int :: String -> Var
int = Var tint

arrayInt :: String -> Var
arrayInt = Var (tarray tint)

arrayBool :: String -> Var
arrayBool = Var (tarray tbool)

-- Numeral operators
data NBinOp = NAdd
            | NSub
            | NMul
            | NDiv
            | NRem

instance Show NBinOp where
    show NAdd = "+"
    show NSub = "-"
    show NMul = "*"
    show NDiv = "/"
    show NRem = "%"

-- Numeral expression
data NExpr = NConst Int -- numeral constant
           | NVar Var -- numeral variable
           | NOp NBinOp NExpr NExpr -- left OP right
           | NArray Var NExpr -- Var[NExpr]

instance Show NExpr where
    show (NConst n) = show n
    show (NVar v) = show v
    show (NOp o l r) = "(" ++ show l ++ show o ++ show r ++ ")"
    show (NArray v e) = show v ++ "[" ++ show e ++ "]"

instance Num NExpr where
    l + r = NOp NAdd l r
    l - r = NOp NSub l r
    l * r = NOp NMul l r
    abs = undefined
    signum = undefined
    fromInteger i = NConst (fromIntegral i)

-- Substitutes free occurences of v1 in e with v2
subfreen :: NExpr -> Var -> Var -> NExpr
subfreen e@(NConst _) _ _ = e
subfreen e@(NVar v) v1 v2 = if v == v1 then NVar v2 else e
subfreen (NOp o l r) v1 v2 = NOp o (subfreen l v1 v2) (subfreen r v1 v2)
subfreen (NArray v i) v1 v2 = if v == v1 then NArray v2 (subfreen i v1 v2) else NArray v (subfreen i v1 v2)

-- Numeral comparison operators
data CBinOp = CEqual
             | CLess
             | CGreater
             | CLeq
             | CGeq

instance Show CBinOp where
    show CEqual = "="
    show CLess = "<"
    show CGreater = ">"
    show CLeq = "<="
    show CGeq = ">="

-- Logical operators
data LBinOp = LEquiv
            | LAnd
            | LOr
            | LImpl

instance Show LBinOp where
    show LEquiv = "="
    show LAnd = "^"
    show LOr = "|"
    show LImpl = "=>"

-- Logical expression
data LExpr = LConst Bool -- True/False
           | LVar Var -- named variable
           | LOp LBinOp LExpr LExpr -- left OP right
           | LComp CBinOp NExpr NExpr -- left OP right
           | LNot LExpr -- NOT op
           | LAll Var LExpr LExpr -- (ALL v: g: s)
           | LAny Var LExpr LExpr -- (ANY v: g: s)
           | LArray Var NExpr -- Var[NExpr] 

instance Show LExpr where
    show (LConst v) = show v
    show (LVar v) = show v
    show (LOp o l r) = "(" ++ show l ++ show o ++ show r ++ ")"
    show (LComp o l r) = "(" ++ show l ++ show o ++ show r ++ ")"
    show (LNot e) = "~" ++ show e
    show (LAll v g s) = "(ALL " ++ show v ++ ": " ++ show g ++ ": " ++ show s ++ ")"
    show (LAny v g s) = "(ANY " ++ show v ++ ": " ++ show g ++ ": " ++ show s ++ ")"
    show (LArray v e) = show v ++ "[" ++ show e ++ "]"

-- Substitutes free occurences of v1 in e with v2
subfreel :: LExpr -> Var -> Var -> LExpr
subfreel e@(LConst _) _ _ = e
subfreel e@(LVar v) v1 v2 = if v == v1 then LVar v2 else e
subfreel (LOp o l r) v1 v2 = LOp o (subfreel l v1 v2) (subfreel r v1 v2)
subfreel (LComp o l r) v1 v2 = LComp o (subfreen l v1 v2) (subfreen r v1 v2)
subfreel (LNot e) v1 v2 = LNot (subfreel e v1 v2)
subfreel e@(LAll v g s) v1 v2 = if v == v1 then e else (LAll v2 (subfreel g v1 v2) (subfreel s v1 v2))
subfreel e@(LAny v g s) v1 v2 = if v == v1 then e else (LAny v2 (subfreel g v1 v2) (subfreel s v1 v2))
subfreel (LArray v i) v1 v2 = if v == v1 then LArray v2 (subfreen i v1 v2) else LArray v (subfreen i v1 v2)

-- Shorthand for (0 <= i < N)
range :: NExpr -> Var -> NExpr -> LExpr
range b v e = LOp LAnd (LComp CLeq b (NVar v)) (LComp CLess (NVar v) e)

vari :: Var
vari = int "i"

varN :: Var
varN = int "N"

vara :: Var
vara = arrayInt "a"

anyTest :: LExpr
anyTest = LAny vari (range (NConst 0) vari (NVar varN)) (LOp LEquiv (LArray vara (NVar vari)) (LConst True))

data Expr = LExpr LExpr
          | NExpr NExpr

instance Show Expr where
    show (LExpr e) = show e
    show (NExpr e) = show e

-- Substitutes free occurences of v1 in e with v2
subfree :: Expr -> Var -> Var -> Expr
subfree (LExpr e) v1 v2 = LExpr (subfreel e v1 v2)
subfree (NExpr e) v1 v2 = NExpr (subfreen e v1 v2)

data Statement = Skip
               | IfElse LExpr Statement Statement
               | Sequence Statement Statement
               | Assignment Var Expr
               | While LExpr Statement
               deriving Show

-- Combine a sequence of statements in a single statement
scomb :: [Statement] -> Statement
scomb [] = Skip
scomb [s] = s
scomb (s:ss) = Sequence s (scomb ss)

-- Weakest precondition
wp :: Statement -> LExpr -> LExpr
wp Skip q = q
wp (IfElse g s1 s2) q = LOp LOr (LOp LImpl g (wp s1 q)) (wp s2 q)
wp (Sequence s1 s2) q = wp s1 (wp s2 q)
wp (Assignment v e) q = undefined 

{-
instance Show Statement where
    show Skip = "skip"
    show (IfElse g s1 s2) = "if " ++ show g ++ " then " ++ show s1 ++ " else " ++ show s2
    show (Sequence s1 s2) = show s1 ++ ";" ++ show s2
    show (Assignment v e) = show v ++ ":=" ++ show e

instance Show Variable where
    show (Var x) = x

instance Show Expression where
    show (Expr x) = x
-}

