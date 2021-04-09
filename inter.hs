{-# LANGUAGE DataKinds #-}
import Data.Function ()
import Data.List ()
import Data.Map ( (!), findMax, insert, null, singleton, Map, fromList, empty )

type Var = String
type Loc = Int
type Store = Map Loc Val
type VEnv = Map String Loc
type Proc = Val -> Store -> Store
type FEnv = Map String Proc

infix 1 :=

data Val
  = String String
  | Int Int
  | Bool Bool
  deriving (Show, Eq, Ord)

andVal :: Val -> Val -> Val
andVal (Bool a) (Bool b) = Bool (a && b)
andVal a b = undefined
orVal :: Val -> Val -> Val
orVal (Bool a) (Bool b) = Bool (a || b)
orVal a b = undefined
notVal :: Val -> Val
notVal (Bool a) = Bool (not a)
notVal a = undefined
addVal :: Val -> Val -> Val
addVal (Int a) (Int b) = Int (a + b)
addVal (String a) (String b) = String (a ++ b)
subVal :: Val -> Val -> Val
subVal (Int a) (Int b) = Int (a - b)
mulVal :: Val -> Val -> Val
mulVal (Int a) (Int b) = Int (a * b)
divVal :: Val -> Val -> Val
divVal (Int a) (Int b) = Int (div a b)
castBool :: Val -> Bool
castBool (Bool a) = a
castBool a = undefined

data Exp
  = V Var
  | C Val
  | Add Exp Exp     -- addition                                                     
  | Sub Exp Exp     -- subtraction                                                  
  | Mul Exp Exp     -- multiplication                                               
  | Div Exp Exp     -- division
  | Equal Exp Exp       -- equal
  | Less Exp Exp        -- less
  | Or Exp Exp        -- or
  | And Exp Exp       -- and
  | Not Exp            -- not

newtype Ref = Ref Var             -- referance 

data Decl
  = DeclVar Var Decl               -- variable declaration
  | DeclFunc Var Var Stmt Decl     -- function declaration
  | EmptyDecl

data Stmt
  = Var := Exp                  -- assignment                                                
  | SeqStmt Stmt Stmt           -- sequence
  | IfStmt Exp Stmt            -- if
  | IfElseStmt Exp Stmt Stmt   -- if else
  | WhileStmt Exp Stmt         -- while
  | SkipStmt                    -- skip
  | BlockStmt Decl Stmt         -- block statement
  | Func Var Exp Var            -- function 

newloc :: Store -> Loc
newloc s = if Data.Map.null s
           then 0
           else fst(findMax s) + 1

semE :: Exp -> VEnv -> Store -> Val
semE (C x) v s = x
semE (V x) v s = s ! (v ! x)
semE (Add e1 e2) v s = addVal (semE e1 v s) (semE e2 v s)
semE (Sub e1 e2) v s = subVal (semE e1 v s) (semE e2 v s)
semE (Mul e1 e2) v s = mulVal (semE e1 v s) (semE e2 v s)
semE (Div e1 e2) v s = divVal (semE e1 v s) (semE e2 v s)
semE (Equal e1 e2) v s = Bool (semE e1 v s == semE e2 v s)
semE (Less e1 e2) v s = Bool (semE e1 v s < semE e2 v s)
semE (Or e1 e2) v s = orVal (semE e1 v s) (semE e2 v s)
semE (And e1 e2) v s = andVal (semE e1 v s) (semE e2 v s)
semE (Not e) v s = notVal (semE e v s)

--TODO FUNC
semD :: Decl -> VEnv -> FEnv -> Store -> (VEnv, FEnv, Store)
semD EmptyDecl v f s = (v, f, s)
semD (DeclVar x d) v f s =
  semD d vd f sd where
    vd = Data.Map.insert x l v
    sd = Data.Map.insert l (Int 0) s
    l = newloc s
semD (DeclFunc x a s1 d) v f s = semD d v fd s where
  fd = Data.Map.insert x func f
  l = newloc s
  func arg s = semS s1 vf fd sf where
    vf = Data.Map.insert x l v
    sf = Data.Map.insert l arg s

semS :: Stmt -> VEnv -> FEnv -> Store -> Store
semS SkipStmt v f s = s
semS (x := e) v f s = Data.Map.insert (v ! x) (semE e v s) s
semS (SeqStmt s1 s2) v f s = semS s2 v f (semS s1 v f s)
semS (IfStmt e s1) v f s =
       if castBool (semE e v s) then semS s1 v f s else s
semS (IfElseStmt e s1 s2) v f s =
       if castBool (semE e v s) then semS s1 v f s else semS s2 v f s
semS (WhileStmt e s1) v f s =
       if castBool (semE e v s)
       then semS (WhileStmt e s1) v f (semS s1 v f s)
       else s
semS (BlockStmt d s1) v f s =
        semS s1 vd fd sd where
          (vd, fd, sd) = semD d v f s

powerStmt :: Int -> Int -> Stmt
powerStmt a b =
  BlockStmt
    (DeclVar "x"
      (DeclVar "i"
        (DeclVar "n"
          (DeclVar "c" EmptyDecl))))
    (SeqStmt
      (SeqStmt ("x" := C (Int 1))
        (SeqStmt ("i" := C (Int 0))
          (SeqStmt ("n" := C (Int b)) ("c" := C (Int a)))))
      (IfElseStmt
        (Less (V "n") (C (Int 0)))
        ("x" := C (Int (-1)))
        (WhileStmt
          (Less (V "i") (V "n"))
          (SeqStmt
            ("x" := Mul (V "x") (V "c"))
            ("i" := Add (V "i") (C (Int 1)))))))

personStmt :: String -> String -> Stmt
personStmt a b =
    BlockStmt
      (DeclVar "s1"
        (DeclVar "s2"
          (DeclVar "b"
            (DeclVar "w"
              (DeclVar "out" EmptyDecl)))))
      (SeqStmt
        (SeqStmt ("s1" := C (String ";; Name: "))
          (SeqStmt ("s2" := C (String ";; Surname: "))
            (SeqStmt
              ("b" := C (String "NoName"))
              ("w" := C (String ";; I dont belive that")))))
          (SeqStmt ("out" := Add (V "s1") (C (String a)))
            (SeqStmt ("out" := Add (V "out") (V "s2"))
             (SeqStmt ("out" := Add (V "out") (C (String b)))
              (IfStmt
                (Or (Equal (C (String a)) (V "b")) (Equal (C (String b)) (V "b")))
                ("out" := Add (V "out") (V "w")))))))


power :: Int -> Int -> Val
person :: String -> String -> Val


power a b = x where
  store = semS (powerStmt a b) Data.Map.empty Data.Map.empty Data.Map.empty
  x = store ! 0

person a b = x where
  store = semS (personStmt a b) Data.Map.empty Data.Map.empty Data.Map.empty
  x = store ! 4