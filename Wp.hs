-- Type primitives
data Primitive = PBool
               | PInt
               deriving Eq

instance Show Primitive where
    show PBool = "bool"
    show PInt  = "int"

-- Types (primitive + [primitive]), no array nesting allowed.
data Type = TPrim Primitive
          | TArray Primitive
          deriving Eq

instance Show Type where
    show (TPrim p)  = show p
    show (TArray p) = "[" ++ show p ++ "]"

-- Handy shorthand functions
tbool :: Type
tbool = TPrim PBool

tint :: Type
tint = TPrim PInt

tarray :: Type -> Type
tarray (TPrim p)  = TArray p
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
    show (NConst n)   = show n
    show (NVar v)     = show v
    show (NOp o l r)  = "(" ++ show l ++ show o ++ show r ++ ")"
    show (NArray v e) = show v ++ "[" ++ show e ++ "]"

instance Num NExpr where
    l + r = NOp NAdd l r
    l - r = NOp NSub l r
    l * r = NOp NMul l r
    abs = undefined
    signum = undefined
    fromInteger i = NConst (fromIntegral i)

-- Substitutes free occurences of v in e1 with e2
subfreen :: NExpr -> Var -> NExpr -> NExpr
subfreen (NConst nc) _ _    = NConst nc
subfreen (NVar nv) v e      = if nv == v then e else NVar nv
subfreen (NOp no nl nr) v e = NOp no (subfreen nl v e) (subfreen nr v e)
subfreen (NArray nv ne) v e = NArray nv (subfreen ne v e)

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

data QOp = QAll | QExists

instance Show LBinOp where
    show LEquiv = "="
    show LAnd = "^"
    show LOr = "|"
    show LImpl = "=>"

instance Show QOp where
  show QAll = "ALL"
  show QExists = "EXISTS"

-- Logical expression
data LExpr = LConst Bool -- True/False
           | LVar Var -- named variable
           | LOp LBinOp LExpr LExpr -- left OP right
           | LComp CBinOp NExpr NExpr -- left OP right
           | LNot LExpr -- NOT op
           | LQuant QOp Var LExpr LExpr -- (QOp v: g: s)
           | LArray Var NExpr -- Var[NExpr]

instance Show LExpr where
    show (LConst v)       = show v
    show (LVar v)         = show v
    show (LOp o l r)      = "(" ++ show l ++ show o ++ show r ++ ")"
    show (LComp o l r)    = "(" ++ show l ++ show o ++ show r ++ ")"
    show (LNot e)         = "~" ++ show e
    show (LQuant o v g s) = "(" ++ show o ++ " " ++ show v ++ ": " ++ show g ++ ": " ++ show s ++ ")"
    show (LArray v e)     = show v ++ "[" ++ show e ++ "]"

subfreel :: LExpr -> Var -> Expr -> LExpr
subfreel (LConst lc) _ _          = LConst lc
subfreel (LVar lv) v e            = if lv == v then lexpr e else LVar lv
subfreel (LOp lo ll lr) v e       = LOp lo (subfreel ll v e) (subfreel lr v e)
subfreel (LComp lo ll lr) v e     = LComp lo (subfreen ll v (nexpr e)) (subfreen lr v (nexpr e))
subfreel (LNot le) v e            = LNot (subfreel le v e)
subfreel (LQuant lo lv ll lr) v e = if lv == v then LQuant lo lv ll lr else LQuant lo lv (subfreel ll v e) (subfreel lr v e)
subfreel (LArray lv le) v e       = LArray lv (subfreen le v (nexpr e))

true :: LExpr
true = LConst True

false :: LExpr
false = LConst False

{-
-- Substitutes free occurences of v1 in e with v2
subfreel :: LExpr -> Var -> LExpr -> LExpr
subfreel e@(LConst _) _ _ = e
subfreel e@(LVar v) v1 v2 = if v == v1 then v2 else e
subfreel (LOp o l r) v1 v2 = LOp o (subfreel l v1 v2) (subfreel r v1 v2)
subfreel (LComp o l r) v1 v2 = LComp o (subfreen l v1 v2) (subfreen r v1 v2)
subfreel (LNot e) v1 v2 = LNot (subfreel e v1 v2)
subfreel e@(LAll v g s) v1 v2 = if v == v1 then e else (LAll v2 (subfreel g v1 v2) (subfreel s v1 v2))
subfreel e@(LAny v g s) v1 v2 = if v == v1 then e else (LAny v2 (subfreel g v1 v2) (subfreel s v1 v2))
subfreel (LArray v i) v1 v2 = if v == v1 then LArray v2 (subfreen i v1 v2) else LArray v (subfreen i v1 v2)
-}

-- Shorthand for (0 <= i < N)
range :: NExpr -> Var -> NExpr -> LExpr
range b v e = LOp LAnd (LComp CLeq b (NVar v)) (LComp CLess (NVar v) e)

vari :: Var
vari = int "i"

vark :: Var
vark = int "k"

varx :: Var
varx = int "x"

varN :: Var
varN = int "N"

vara :: Var
vara = arrayInt "a"

found :: Var
found = bool "found"

exists :: Var -> LExpr -> LExpr -> LExpr
exists = LQuant QExists

--anyTest :: LExpr
--anyTest = lany vari (range (NConst 0) vari (NVar varN)) (LOp LEquiv (LArray vara (NVar vari)) (LConst True))

inv1a :: LExpr
inv1a = exists vark (range (NConst 0) vark (NVar vari)) (LOp LEquiv (LArray vara (NVar vark)) (LVar varx))

inv1 :: LExpr
inv1 = LOp LEquiv (LVar found) inv1a

inv2 :: LExpr
inv2 = LOp LAnd (LComp CLeq (NConst 0) (NVar vari)) (LComp CLeq (NVar vari) (NVar varN))

inv :: LExpr
inv = LOp LAnd inv1 inv2

data Expr = LExpr LExpr
          | NExpr NExpr

instance Show Expr where
    show (LExpr e) = show e
    show (NExpr e) = show e

lexpr :: Expr -> LExpr
lexpr (LExpr e) = e
lexpr (NExpr e) = error $ "type of Expr is not LExpr: " ++ show e

nexpr :: Expr -> NExpr
nexpr (LExpr e) = error $ "type of Expr is not NExpr: " ++ show e
nexpr (NExpr e) = e

-- Substitutes free occurences of v1 in e with v2
subfree :: Expr -> Var -> Expr -> Expr
subfree (LExpr le) v e = LExpr (subfreel le v e)
subfree (NExpr ne) v e = NExpr (subfreen ne v (nexpr e))

data Statement = Skip
               | IfElse LExpr Statement Statement
               | Sequence Statement Statement
               | Assignment Var Expr
               | While LExpr Statement

instance Show Statement where
    show Skip = "skip"
    show (IfElse g s1 s2) = "if " ++ show g ++ " then " ++ show s1 ++ " else " ++ show s2
    show (Sequence s1 s2) = show s1 ++ ";" ++ show s2
    show (Assignment v e) = show v ++ ":=" ++ show e

bodyfound :: Statement
bodyfound = Assignment found (LExpr (LOp LOr (LVar found) (LOp LEquiv (LArray vara (NVar vari)) (LVar varx))))

bodyinc :: Statement
bodyinc = Assignment vari (NExpr (NOp NAdd (NVar vari) (NConst 1)))

bodyloop :: Statement
bodyloop = Sequence bodyfound bodyinc

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
wp (Assignment v e) q = subfreel q v e