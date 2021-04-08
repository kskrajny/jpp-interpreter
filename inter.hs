{-# LANGUAGE DataKinds #-}
import Data.Function
import Data.List
import Data.Map ( (!), findMax, insert, null, singleton, Map, fromList )

type Var = String
type Loc = Int
type Store = Map Loc Int
type VEnv = Map String Loc

infix 1 :=

data Exp
  = N Int           -- constant                                                    
  | V Var           -- variable                                                     
  | Add Exp Exp     -- addition                                                     
  | Sub Exp Exp     -- subtraction                                                  
  | Mul Exp Exp     -- multiplication                                               
--  | Div Exp Exp     -- division

data BExp
  = B Bool              -- variable 
  | Equal Exp Exp       -- equal
  | Less Exp Exp        -- less
  | Or BExp BExp        -- or
  | And BExp BExp       -- and
  | Not BExp            -- not

newtype Ref = Ref Var             -- referance 

data Arg
  = EmptyArg
  | RefArg Ref Arg
  | VarArg Var Arg

data Decl
  = DeclVar Var Decl               -- variable declaration
  | DeclFunc Var Arg Stmt Decl     -- function declaration
  | EmptyDecl

data Stmt
  = Var := Exp                  -- assignment                                                
  | SeqStmt Stmt Stmt           -- sequence
  | IfStmt BExp Stmt            -- if
  | IfElseStmt BExp Stmt Stmt   -- if else
  | WhileStmt BExp Stmt         -- while
  | SkipStmt                    -- skip
  | BlockStmt Decl Stmt         -- block statement

newloc :: Store -> Loc
newloc s = if Data.Map.null s
           then 0
           else fst(findMax s) + 1

semE :: Exp -> VEnv -> Store -> Int
semE (N x) v s = x
semE (V x) v s = s ! (v ! x)
semE (Add e1 e2) v s = semE e1 v s + semE e2 v s
semE (Sub e1 e2) v s = semE e1 v s - semE e2 v s
semE (Mul e1 e2) v s = semE e1 v s * semE e2 v s
-- semE (e1 :/: e2) v s = semE e1 v s / semE e2 v s

semB :: BExp -> VEnv -> Store -> Bool
semB (B b) v s = b
semB (Equal e1 e2) v s = semE e1 v s == semE e2 v s
semB (Less e1 e2) v s = semE e1 v s < semE e2 v s
semB (Or b1 b2) v s = semB b1 v s || semB b2 v s
semB (And b1 b2) v s = semB b1 v s && semB b2 v s
semB (Not b) v s = not (semB b v s)

--TODO FUNC
semD :: Decl -> VEnv -> Store -> (VEnv, Store)
semD EmptyDecl v s = (v, s)
semD (DeclVar x d) v s =
  semD d vd sd where
    vd = Data.Map.insert x l v
    sd = Data.Map.insert l 0 s
    l = newloc s
semD (DeclFunc x a s1 d) v s = (v, s)

semS :: Stmt -> VEnv -> Store -> Store
semS SkipStmt v s = s
semS (x := e) v s = Data.Map.insert (v ! x) (semE e v s) s
semS (SeqStmt s1 s2) v s = semS s2 v (semS s1 v s)
semS (IfStmt b s1) v s =
       if semB b v s then semS s1 v s else s
semS (IfElseStmt b s1 s2) v s =
       if semB b v s then semS s1 v s else semS s2 v s
semS (WhileStmt b s1) v s =
       if semB b v s
       then semS (WhileStmt b s1) v (semS s1 v s)
       else s
semS (BlockStmt d s1) v s =
        semS s1 vd sd where
          (vd, sd) = semD d v s

powerStmt :: Stmt
powerStmt =
  WhileStmt (Less (V "i") (V "n")) (
    SeqStmt
      ("x" := Mul (V "x") (V "c"))
      ("i" := Add (V "i") (N 1))
  )

power :: Int -> Int -> Int
power a b = x where
  store = semS powerStmt
    (Data.Map.fromList [("x", 0), ("i", 1), ("n", 2), ("c", 3)])
    (Data.Map.fromList [(0, 1), (1, 0), (2, b), (3, a)])
  x = store ! 0