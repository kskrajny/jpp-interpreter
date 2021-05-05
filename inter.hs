{-# LANGUAGE BlockArguments #-}
module Inter where
import Data.Function ()
import Data.List ()
import Data.Map ( (!), findMax, insert, null, singleton, Map, fromList, empty, member )
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
-- TODO -- Errors, Static typing

-- LANGUAGE --

type Var = String
type Loc = Int
type Store = Map Loc Val
type VEnv = Map String Loc
type Proc = Val -> Store -> Store
type FEnv = Map String Proc

data Val
  = String String
  | Int Integer
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
  = AssignStmt Var Exp                  -- assignment                                                
  | SeqStmt Stmt Stmt           -- sequence
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
semS (AssignStmt x e) v f s = Data.Map.insert (v ! x) (semE e v s) s
semS (SeqStmt s1 s2) v f s = semS s2 v f (semS s1 v f s)
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

-- PARSER --

languageDef =
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "print"
                                     , "run"
                                     , "func"
                                     , "var"
                                     , "declare"
                                     , "end declare"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":=", "=="
                                     , "<", "and", "or", "not"
                                     ]
           }
lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
stringLiteral = Token.stringLiteral lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

mainParser :: Parser Stmt
mainParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement
          <|> sequenceOfStmt


helpseq :: [Stmt] -> Stmt
helpseq = foldr SeqStmt SkipStmt

sequenceOfStmt =
  do list <- sepBy1 statement' semi
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else helpseq list

statement' :: Parser Stmt
statement' = ifElseStmt
           <|> whileStmt
           <|> assignStmt
           <|> skipStmt
           <|> printStmt
           <|> blockStmt
           <|> funcStmt

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- expression
     reserved "do"
     WhileStmt cond <$> statement

assignStmt :: Parser Stmt
assignStmt =
  do var <- identifier
     reservedOp ":="
     AssignStmt var <$> expression

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return SkipStmt

printStmt :: Parser Stmt
printStmt =
  do reserved "print"
     PrintStmt <$> expression

ifElseStmt :: Parser Stmt
ifElseStmt =
  do reserved "if"
     cond <- expression
     reserved "then"
     stmt <- statement
     reserved "else" 
     IfElseStmt cond stmt <$> statement

blockStmt :: Parser Stmt
blockStmt =
  do reserved "declare"
     decl <- declaration
     BlockStmt decl <$> statement

funcStmt :: Parser Stmt
funcStmt = 
  do reserved "run"
     var <- identifier
     FuncStmt var <$> expression

declaration :: Parser Decl
declaration = declVar
      <|> declFunc
      <|> (reserved "end declare" >> return EmptyDecl)

declVar :: Parser Decl
declVar =
  do reserved "var"
     var <- identifier
     semi
     DeclVar var <$> declaration

declFunc :: Parser Decl
declFunc =
  do reserved "func"
     var1 <- identifier
     var2 <- identifier
     var3 <- identifier
     stmt <- statement
     DeclFunc var1 var2 var3 stmt <$> declaration

expression :: Parser Exp
expression = buildExpressionParser operators aTerm
    <|> aTerm


operators = [[Infix  (reservedOp "*"   >> return Mul) AssocLeft,
                Infix  (reservedOp "/"   >> return Div) AssocLeft]
             , [Infix  (reservedOp "+"   >> return Add) AssocLeft,
                Infix  (reservedOp "-"   >> return Sub) AssocLeft]
             , [Prefix (reservedOp "not" >> return Not)]
             , [Infix  (reservedOp "and" >> return And) AssocLeft,
                Infix  (reservedOp "or"  >> return Or) AssocLeft]
             , [Infix (reservedOp "==" >> return Equal) AssocLeft,
                Infix (reservedOp "<" >> return Less) AssocLeft]
             ]

aTerm = parens expression
     <|> C <$> value
     <|> V <$> identifier

value :: Parser Val
value = String <$> stringLiteral
    <|> Int <$> integer
    <|> (reserved "true"  >> return (Bool True))
    <|> (reserved "false" >> return (Bool False))

relation = reservedOp "<" >> return Less

parseString :: String -> Stmt
parseString str =
  case parse mainParser "" str of
    Left e  -> error $ show e
    Right r -> r


parseFile :: String -> IO()
parseFile file =
  do program  <- readFile file
     case parse mainParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> exec r
