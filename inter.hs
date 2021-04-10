module Inter where
import Data.Function ()
import Data.List ()
import Data.Map ( (!), findMax, insert, null, singleton, Map, fromList, empty, member )

-- LANGUAGE --

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
  deriving(Eq, Ord)

instance Show Val where
  show (String a) = a
  show (Bool a) = show a
  show (Int a) = show a

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

data Decl
  = DeclVar Var Decl               -- variable declaration
  | DeclFunc Var Var Var Stmt Decl     -- function declaration
  | EmptyDecl

data Stmt
  = Var := Exp                  -- assignment                                                
  | SeqStmt Stmt Stmt           -- sequence
  | IfStmt Exp Stmt            -- if
  | IfElseStmt Exp Stmt Stmt   -- if else
  | WhileStmt Exp Stmt         -- while
  | SkipStmt                    -- skip
  | BlockStmt Decl Stmt         -- block statement
  | FuncStmt Var Exp                -- function
  | PrintStmt Exp                   -- print


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
toString :: Val -> Val
toString (Int a) = String (show a)
toString (Bool a) = String (show a)
toString a = a

newloc :: Store -> Loc
newloc s = if Data.Map.null s
           then 1
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
semE (And e1 e2) v s = andVal (semE e1 v s) (semE e2 v s)
semE (Or e1 e2) v s = orVal (semE e1 v s) (semE e2 v s)
semE (Not e) v s = notVal (semE e v s)

--TODO FUNC
semD :: Decl -> VEnv -> FEnv -> Store -> (VEnv, FEnv, Store)
semD EmptyDecl v f s = (v, f, s)
semD (DeclVar x d) v f s =
  semD d vd f sd where
    vd = Data.Map.insert x l v
    sd = Data.Map.insert l (Int 0) s
    l = newloc s
semD (DeclFunc x in_arg out_arg s1 d) v f s = semD d v fd s where
  fd = Data.Map.insert x func f
  func a s = semS s1 vf fd sf where
    l1 = newloc s
    vf = Data.Map.insert "return" (v ! out_arg) (Data.Map.insert in_arg l1 v)
    sf = Data.Map.insert (v ! out_arg) undefined (Data.Map.insert l1 a s)


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
semS (FuncStmt func x) v f s = (f ! func) (semE x v s) s
semS (PrintStmt e) v f s = if Data.Map.member 0 s
  then Data.Map.insert 0
    (addVal
      (addVal (toString (s ! 0)) (toString (semE e v s)))
      (String "\n")) s
  else Data.Map.insert 0 (toString (semE e v s)) s

exec :: Stmt -> IO()
exec s = putStr (
    show (
      semS s
        Data.Map.empty
        Data.Map.empty 
        (Data.Map.fromList [(0, String "")])! 0))

-- EXAMPLES --

printStmt :: Stmt
printStmt =
  SeqStmt
    (PrintStmt (C (String "Słowo")))
    (SeqStmt
      (PrintStmt (C (Int 1)))
      (PrintStmt (C (Bool True))))

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
        (PrintStmt (C (String "Cant count it")))
        (SeqStmt
          (WhileStmt
            (Less (V "i") (V "n"))
            (SeqStmt
              ("x" := Mul (V "x") (V "c"))
              ("i" := Add (V "i") (C (Int 1)))))
          (PrintStmt (V "x")))))

personStmt :: String -> String -> Stmt
personStmt a b =
    BlockStmt
      (DeclVar "s1"
        (DeclVar "s2"
          (DeclVar "b"
            (DeclVar "w"
              (DeclVar "out" EmptyDecl)))))
      (SeqStmt
        (SeqStmt
          (SeqStmt ("s1" := C (String "; Name: "))
            (SeqStmt ("s2" := C (String "; Surname: "))
              (SeqStmt
                ("b" := C (String "NoName"))
                ("w" := C (String "; I dont belive that")))))
            (SeqStmt ("out" := Add (V "s1") (C (String a)))
              (SeqStmt ("out" := Add (V "out") (V "s2"))
              (SeqStmt ("out" := Add (V "out") (C (String b)))
                (IfStmt
                  (Or (Equal (C (String a)) (V "b")) (Equal (C (String b)) (V "b")))
                  ("out" := Add (V "out") (V "w")))))))
        (PrintStmt (V "out")))

funcStmt :: Int -> Stmt
funcStmt x =
  BlockStmt
    (DeclVar "y"
      (DeclFunc "f" "x" "y"
      (SeqStmt
        ("return" := V "x")
        (WhileStmt
          (Less (V "return") (C (Int 100)))
          ("return" := Mul (V "return") (V "x"))))
          EmptyDecl))
    (SeqStmt
      (FuncStmt "f" (C (Int x)))
      (PrintStmt (V "y")))

rekStmt :: Int -> Int -> Stmt
rekStmt a b =
  BlockStmt
    (DeclVar "y"
      (DeclFunc "f" "x" "y"
        (IfStmt
          (Less (V "x") (C (Int b)))
          (SeqStmt
            (SeqStmt
              ("x" := Add (V "x") (C (Int 1)))
              (FuncStmt "f" (V "x")))
            (PrintStmt (V "x"))))
          EmptyDecl))
      (FuncStmt "f" (C (Int a)))
  
-- is this example os staic typing already ?
badStmt :: Stmt
badStmt =
  SeqStmt
    (PrintStmt (C (String "Start")))
    (IfStmt
      (Add (C (Int 2)) (C (Int 2)))
      (PrintStmt (C (String "WTF"))))