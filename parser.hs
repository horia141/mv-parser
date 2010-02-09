module Main where

import Data.List
import Data.Bits

import Data.Char
import Numeric

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

data MVExpr
    = Func {
        funcName :: String,
        funcArgList :: [MVExpr]}
    | Path {
        pathInst :: String,
        pathName :: String}
    | Symb {
        symbName :: String}
    | Numb {
        numbValue :: String}
    deriving (Show)

data MVDefs
    = Def {
        defName :: String,
        defValue :: MVExpr,
        defBuiltin :: Bool}
    | Mod {
        modName :: String,
        modAttrList :: [MVUnitModAttr],
        modInList :: [MVUnitModIn],
        modOutList :: [MVUnitModOut],
        modIoList :: [MVUnitModIo],
        modInstList :: [MVUnitModInst],
        modBuiltin :: Bool}
    | Fsm {
        fsmName :: String,
        fsmAttrList :: [MVUnitFsmAttr],
        fsmInList :: [MVUnitFsmIn],
        fsmOutList :: [MVUnitFsmOut],
        fsmStateList :: [MVUnitFsmState],
        fsmBuiltin :: Bool}
    deriving (Show)

data MVUnitModAttr
    = ModAttr {
        modAttrName :: String}
    deriving (Show)

data MVUnitModIn
    = ModIn {
        modInName :: String,
        modInSize :: MVExpr}
    deriving (Show)

data MVUnitModOut
    = ModOut {
        modOutName :: String,
        modOutSize :: MVExpr,
        modOutSink :: MVExpr}
    deriving (Show)

data MVUnitModIo
    = ModIo {
        modIoName :: String,
        modIoSize :: MVExpr,
        modIoSink :: MVExpr,
        modIoWriteEn :: MVExpr}
    deriving (Show)

data MVUnitModInst
    = ModInst {
        modInstKind :: String,
        modInstName :: String,
        modInstAttrList :: [MVExpr],
        modInstNVPairList :: [MVUnitModNVPair]}
    deriving (Show)

data MVUnitModNVPair
    = ModNVPair {
        modNVPairName :: String,
        modNVPairValue :: MVExpr}
    deriving (Show)

data MVUnitFsmAttr
    = FsmAttr {
        fsmAttrName :: String}
    deriving (Show)

data MVUnitFsmIn
    = FsmIn {
        fsmInName :: String,
        fsmInSize :: MVExpr}
    deriving (Show)

data MVUnitFsmOut
    = FsmOut {
        fsmOutName :: String,
        fsmOutSize :: MVExpr,
        fsmOutOffset :: MVExpr}
    deriving (Show)

data MVUnitFsmState
    = FsmState {
        fsmStateName :: String,
        fsmStateOutput :: MVExpr,
        fsmStateChangeList :: [MVUnitFsmChange]}
    deriving (Show)

data MVUnitFsmChange
    = FsmChange {
        fsmChangeState :: String,
        fsmChangeTest :: MVExpr}
    deriving (Show)

lexer :: TokenParser ()
lexer = makeTokenParser (LanguageDef {
                           commentStart    = "",
                           commentEnd      = "",
                           commentLine     = "#",
                           nestedComments  = True,
                           identStart      = letter <|> char '_',
                           identLetter     = letter <|> digit <|> char '_',
                           opStart         = oneOf "",
                           opLetter        = oneOf "",
                           reservedOpNames = [],
                           reservedNames   = ["mod","fsm","def","State"],
                           caseSensitive   = True})

enclose :: (TokenParser () -> Parser [a] -> b) -> Parser a -> b
enclose encloser p = encloser lexer $ sepBy p (whiteSpace lexer)

expr :: Parser MVExpr
expr = (try func) <|> (try numb) <|> (try path) <|> symb

func :: Parser MVExpr
func = parens lexer (do name <- identifier lexer
                        argList <- sepBy expr (whiteSpace lexer)

                        return (Func name argList))

path :: Parser MVExpr
path = do inst <- identifier lexer
          dot lexer
          name <- identifier lexer

          return (Path inst name)

symb :: Parser MVExpr
symb = do name <- identifier lexer

          return (Symb name)

numb :: Parser MVExpr
numb = lexeme lexer (do base <- option 'd' (do base <- oneOf "bodh"
                                               char ':'
                                               return base)

                        value <- case base of
                                   'b' -> many1 (oneOf "zZxX01")
                                   'o' -> many1 (oneOf "zZxX01234567")
                                   'd' -> many1 (oneOf "0123456789")
                                   'h' -> many1 (oneOf "zZxX0123456789aAbBcCdDeEfF")

                        return (Numb (x2b base value)))
    where x2b :: Char -> String -> String
          x2b 'b' input = reverse input
          x2b 'd' input = 
              let intInput = read input :: Integer
              in case intInput of
                   0 -> "0"
                   _ -> doRec intInput
                  where doRec :: Integer -> String
                        doRec 0 = ""
                        doRec num = 
                            case num .&. 1 of
                              0 -> '0':(doRec $ shiftR num 1)
                              1 -> '1':(doRec $ shiftR num 1)
          x2b 'o' input = foldl (++) [] $ reverse $ map (o2b_dig) input
              where o2b_dig 'z' = "zzz";  o2b_dig 'Z' = "zzz";  o2b_dig 'x' = "xxx";  o2b_dig 'X' = "xxx"
                    o2b_dig '0' = "000";  o2b_dig '1' = "001";  o2b_dig '2' = "010";  o2b_dig '3' = "011"
                    o2b_dig '4' = "100";  o2b_dig '5' = "101";  o2b_dig '6' = "110";  o2b_dig '7' = "111"
          x2b 'h' input = foldl (++) [] $ reverse $ map (h2b_dig) input
              where h2b_dig 'z' = "zzzz"; h2b_dig 'Z' = "zzzz"; h2b_dig 'x' = "xxxx"; h2b_dig 'X' = "xxxx"
                    h2b_dig '0' = "0000"; h2b_dig '1' = "0001"; h2b_dig '2' = "0010"; h2b_dig '3' = "0011"
                    h2b_dig '4' = "0100"; h2b_dig '5' = "0101"; h2b_dig '6' = "0110"; h2b_dig '7' = "0111"
                    h2b_dig '8' = "1000"; h2b_dig '9' = "1001"; h2b_dig 'a' = "1010"; h2b_dig 'A' = "1010"
                    h2b_dig 'b' = "1011"; h2b_dig 'B' = "1011"; h2b_dig 'c' = "1100"; h2b_dig 'C' = "1100"
                    h2b_dig 'd' = "1101"; h2b_dig 'D' = "1101"; h2b_dig 'e' = "1110"; h2b_dig 'E' = "1110"
                    h2b_dig 'f' = "1111"; h2b_dig 'F' = "1111"

defs :: Parser MVDefs
defs = def <|> Main.mod <|> fsm

def :: Parser MVDefs
def = do reserved lexer "def"
         name <- identifier lexer
         value <- expr

         return (Def name value False)

mod :: Parser MVDefs
mod = do reserved lexer "mod"
         name <- identifier lexer
         attrList <- enclose squares modAttr
         inList <- enclose squares modIn
         outList <- enclose squares modOut
         ioList <- enclose squares modIo
         instList <- enclose braces modInst

         return (Mod name attrList inList outList ioList instList False)

fsm :: Parser MVDefs
fsm = do reserved lexer "fsm"
         name <- identifier lexer
         attrList <- enclose squares fsmAttr
         inList <- enclose squares fsmIn
         outList <- enclose squares fsmOut
         stateList <- enclose braces fsmState

         return (Fsm name attrList inList outList stateList False)

modAttr :: Parser MVUnitModAttr
modAttr = do name <- identifier lexer

             return (ModAttr name)

modIn :: Parser MVUnitModIn
modIn = do name <- identifier lexer
           size <- parens lexer expr

           return (ModIn name size)

modOut :: Parser MVUnitModOut
modOut = do name <- identifier lexer
            (size,sink) <- parens lexer (do size <- expr
                                            sink <- expr

                                            return (size,sink))

            return (ModOut name size sink)

modIo :: Parser MVUnitModIo
modIo = do name <- identifier lexer
           (size,sink,writeEn) <- parens lexer (do size <- expr
                                                   sink <- expr
                                                   writeEn <- expr

                                                   return (size,sink,writeEn))

           return (ModIo name size sink writeEn)

modInst :: Parser MVUnitModInst
modInst = do kind <- identifier lexer
             attrList <- enclose squares expr
             name <- identifier lexer
             nvPairList <- enclose braces nvPair

             return (ModInst kind name attrList nvPairList)

nvPair :: Parser MVUnitModNVPair
nvPair = do name <- identifier lexer
            value <- expr

            return (ModNVPair name value)

fsmAttr :: Parser MVUnitFsmAttr
fsmAttr = do name <- identifier lexer

             return (FsmAttr name)

fsmIn :: Parser MVUnitFsmIn
fsmIn = do name <- identifier lexer
           size <- parens lexer expr

           return (FsmIn name size)

fsmOut :: Parser MVUnitFsmOut
fsmOut = do name <- identifier lexer
            (size,offset) <- parens lexer (do size <- expr
                                              offset <- expr

                                              return (size,offset))

            return (FsmOut name size offset)

fsmState :: Parser MVUnitFsmState
fsmState = do reserved lexer "State"
              output <- squares lexer expr
              name <- identifier lexer
              changeList <- enclose braces fsmChange

              return (FsmState name output changeList)

fsmChange :: Parser MVUnitFsmChange
fsmChange = do state <- identifier lexer
               test <- option (Func "else" []) expr

               return (FsmChange state test)

mvparser :: Show a => Parser a -> String -> IO ()
mvparser parser input
    = case parse (lexerparser parser) "" input of
        Left err -> do putStr "syntax error at "
                       print err
        Right ix -> do putStrLn (show ix)
    where lexerparser parser = do whiteSpace lexer
                                  res <- parser
                                  eof
                                  return res

_bsSize :: Integer -> Integer
_bsSize num =
    toInteger $ length $ _bsShow num

_bsShow :: Integer -> String
_bsShow num =
    showIntAtBase 2 intToDigit num ""

_bsRead :: String -> Integer
_bsRead str =
    case readF str of
      [(numb,"")] -> numb
      _ -> error ("Invalid input to _bsRead : \"" ++ str ++ "\"!")
    where readF :: ReadS Integer
          readF = readInt 2 (\ x -> x == '0' || x == '1') digitToInt

bsAdd :: Integer -> Integer -> Integer
bsAdd num0 num1 =
    num0 + num1

bsSub :: Integer -> Integer -> Integer
bsSub num0 num1 =
    num0 - num1

bsMul :: Integer -> Integer -> Integer
bsMul num0 num1 =
    num0 * num1

bsDiv :: Integer -> Integer -> Integer
bsDiv num0 num1 =
    div num0 num1

bsMod :: Integer -> Integer -> Integer
bsMod num0 num1 =
    mod num0 num1

bsNeg :: Integer -> Integer
bsNeg num0 =
    (-num0)

bsPos :: Integer -> Integer
bsPos num0 =
    num0

bsLt :: Integer -> Integer -> Integer
bsLt num0 num1 =
    case num0 < num1 of
      True -> 1
      False -> 0

bsLte :: Integer -> Integer -> Integer
bsLte num0 num1 =
    case num0 <= num1 of
      True -> 1
      False -> 0

bsGt :: Integer -> Integer -> Integer
bsGt num0 num1 =
    case num0 > num1 of
      True -> 1
      False -> 0

bsGte :: Integer -> Integer -> Integer
bsGte num0 num1 =
    case num0 >= num1 of
      True -> 1
      False -> 0

bsEq :: Integer -> Integer -> Integer
bsEq num0 num1 =
    case num0 == num1 of
      True -> 1
      False -> 0

bsNeq :: Integer -> Integer -> Integer
bsNeq num0 num1 =
    case num0 /= num1 of
      True -> 1
      False -> 0

bsNot :: Integer -> Integer
bsNot num0 =
    complement num0

bsOr :: Integer -> Integer -> Integer
bsOr num0 num1 = 
    let n0 = num0 /= 0
        n1 = num1 /= 0
    in case n0 || n1 of
         True -> 1
         False -> 0

bsAnd :: Integer -> Integer -> Integer
bsAnd num0 num1 = 
    let n0 = num0 /= 0
        n1 = num1 /= 0
    in case n0 && n1 of
         True -> 1
         False -> 0

bsXor :: Integer -> Integer -> Integer
bsXor num0 num1 = 
    let n0 = num0 /= 0
        n1 = num1 /= 0
    in case (n0 && (not n1)) || ((not n0) && n1) of
         True -> 1
         False -> 0

bsNor :: Integer -> Integer -> Integer
bsNor num0 num1 =
    let n0 = num0 /= 0
        n1 = num1 /= 0
    in case not (n0 || n1) of
         True -> 1
         False -> 0

bsNand :: Integer -> Integer -> Integer
bsNand num0 num1 =
    let n0 = num0 /= 0
        n1 = num1 /= 0
    in case not (n0 && n1) of
         True -> 1
         False -> 0

bsXnor :: Integer -> Integer -> Integer
bsXnor num0 num1 =
    let n0 = num0 /= 0
        n1 = num1 /= 0
    in case not (n0 && (not n1)) || ((not n0) && n1) of
         True -> 1
         False -> 0

bsBwOr :: Integer -> Integer -> Integer
bsBwOr num0 num1 =
    num0 .|. num1

bsBwAnd :: Integer -> Integer -> Integer
bsBwAnd num0 num1 =
    num0 .&. num1

bsBwXor :: Integer -> Integer -> Integer
bsBwXor num0 num1 =
    num0 `xor` num1

bsBwNor :: Integer -> Integer -> Integer
bsBwNor num0 num1 =
    complement (num0 .|. num1)

bsBwNand :: Integer -> Integer -> Integer
bsBwNand num0 num1 =
    complement (num0 .&. num1)

bsBwXnor :: Integer -> Integer -> Integer
bsBwXnor num0 num1 =
    complement (num0 `xor` num1)

bsShl :: Integer -> Integer -> Integer
bsShl num0 num1 =
    shiftL num0 (fromInteger num1)

bsShr :: Integer -> Integer -> Integer
bsShr num0 num1 =
    shiftR num0 (fromInteger num1)

bsCat :: [Integer] -> Integer
bsCat numList =
    foldl cat2 0 numList
    where cat2 :: Integer -> Integer -> Integer
          cat2 num0 num1 = 
              let shiftSize = _bsSize num1
              in (shiftL num0 (fromInteger shiftSize)) + num1

bsDup :: Integer -> Integer -> Integer
bsDup num0 num1 =
    bscat (replicate (fromInteger num1) num0)

bsFill :: Integer -> Integer -> Integer
bsFill num0 num1 =
    bsrange (bsdup num0 (num1 `div` (_bsSize num0) + 1)) 0 num1

bsIndex :: Integer -> Integer -> Integer
bsIndex num0 num1 =
    bsrange num0 num1 1

bsRange :: Integer -> Integer -> Integer -> Integer
bsRange num0 num1 num2 =
    _bsRead $ reverse $ take (fromInteger num2) $ drop (fromInteger num1) $ reverse $ _bsShow num0

indent :: Int -> String -> String
indent tab source = 
    intercalate "\n" $ map ((replicate tab ' ')++) $ lines source

data FuncType
    = Unary (String -> String)
    | Binary (String -> String -> String)
    | Ternary (String -> String -> String -> String)
    | VarArg ([String] -> String)

evaluateCt :: MVExpr -> [MVDefs] -> String
evaluateCt (Func name argList) (toplevel) =
    let argResult = map (flip evaluateCt $ toplevel) argList
    in case find (\ (x,_) -> name == x) funTable of
         Just (name,Unary func) -> if (length argResult) == 1
                                   then func (head argResult)
                                   else error ("Invalid number of arguments for function " ++ name ++ "!")
         Just (name,Binary func) -> if (length argResult) == 2
                                    then func (head argResult) (head $ tail argResult)
                                    else error ("Invalid number of arguments for function " ++ name ++ "!")
         Just (name,Ternary func) -> if (length argResult) == 3
                                     then func (head argResult) (head $ tail argResult) (head $ tail $ tail argResult)
                                     else error ("Invalid number of arguments for function " ++ name ++ "!")
         Just (name,VarArg func) -> func argResult
         Nothing -> error ("The function " ++ name ++ " does not exist!")
       where funTable :: [(String,FuncType)]
             funTable = [("add",Binary add2)]

             matchSize :: String -> String -> (String,String)
             matchSize num0 num1 =
                 let difDist = abs $ (length num0) - (length num1)
                     difList = replicate difDist '0'
                 in if (length num0) > (length num1)
                    then (num0,num1 ++ difList)
                    else (num0 ++ difList,num1)

             add2 :: String -> String -> String
             add2 (num0) (num1) = 
                 let (num0r,num1r) = matchSize num0 num1
                 in add2rec num0r num1r "" False
                 where add2rec :: String -> String -> String -> Bool -> String
                       add2rec ("")        ("")        (result) (False) = reverse result
                       add2rec ("")        ("")        (result) (True)  = reverse $ '1':result
                       add2rec ('0':num0r) ('0':num1r) (result) (False) = add2rec num0r num1r ('0':result) False
                       add2rec ('0':num0r) ('1':num1r) (result) (False) = add2rec num0r num1r ('1':result) False
                       add2rec ('1':num0r) ('0':num1r) (result) (False) = add2rec num0r num1r ('1':result) False
                       add2rec ('1':num0r) ('1':num1r) (result) (False) = add2rec num0r num1r ('0':result) True
                       add2rec ('0':num0r) ('0':num1r) (result) (True)  = add2rec num0r num1r ('1':result) False
                       add2rec ('0':num0r) ('1':num1r) (result) (True)  = add2rec num0r num1r ('0':result) True
                       add2rec ('1':num0r) ('0':num1r) (result) (True)  = add2rec num0r num1r ('0':result) True
                       add2rec ('1':num0r) ('1':num1r) (result) (True)  = add2rec num0r num1r ('1':result) True

             not1 :: String -> String
             not1 (num0) = 
                 
evaluateCt (Path inst name) (toplevel) =
    error ("Can't reference path " ++ inst ++ " " ++ name ++ " in constant context!")
evaluateCt (Symb name) (toplevel) =
    case (find testDef toplevel) of
      Just (Def defName defValue defBuiltin) -> evaluateCt defValue toplevel
      Nothing -> error ("Can't find " ++ name)
    where testDef :: MVDefs -> Bool
          testDef (Def defName defValue defBuiltin) = name == defName
          testDef _ = False
evaluateCt (Numb value) (toplevel) =
    value


generate :: MVDefs -> [MVDefs] -> Int -> String
generate (Def name value builtin) (toplevel) (tab) = 
    indent tab $ "`define " ++ name ++ " " ++ (evaluateCt value toplevel)
generate (Mod name attrList inList outList ioList instList builtin) (toplevel) (tab) =
    indent tab $ "module " ++ name ++ "();" ++ "\n" ++ "endmodule"
generate (Fsm name attrList inList outList stateList builtin) (toplevel) (tab) =
    indent tab ("module " ++ name ++ "(" ++ genParamList ++ ");" ++ "\n" ++
                genAttrs ++ "\n" ++ 
                genIns ++ "\n" ++ 
                genOuts ++ "\n" ++
                "endmodule")
    where genParamList :: String
          genParamList = intercalate "," ((map fsmInName inList) ++ (map fsmOutName outList))

          genAttrs :: String
          genAttrs = unlines $ map genAttr attrList
              where genAttr :: MVUnitFsmAttr -> String
                    genAttr (FsmAttr name) =
                        indent 2 $ "parameter " ++ name ++ ";"

          genIns :: String
          genIns = unlines $ map genIn inList
              where genIn :: MVUnitFsmIn -> String
                    genIn (FsmIn name size) =
                        indent 2 $ "input wire[0:" ++ (evaluateCt size toplevel) ++ "-1] " ++ name ++ ";"

          genOuts :: String
          genOuts = unlines $ map genOut outList
              where genOut :: MVUnitFsmOut -> String
                    genOut (FsmOut name size offset) = 
                        indent 2 $ "output wire[0:" ++ (evaluateCt size toplevel) ++ "-1] " ++ name ++ ";\n" ++
                                   "assign " ++ name ++ " = fsm_output[" ++ (evaluateCt offset toplevel) ++ ":" ++ (evaluateCt size toplevel) ++ "];//wrong"
          
compile :: FilePath -> IO ()
compile path
    = do fileContents <- readFile path
         
         case parse (lexparser (many defs)) "" fileContents of
           Left err -> do putStr "syntax error at "
                          print err
           Right res -> putStrLn (intercalate "\n\n" (map (\ x -> generate x res 0) res))
    where lexparser parser = do whiteSpace lexer
                                res <- parser
                                eof
                                return res

--                          ("sub",Binary sub2),
--                          ("mul",Binary mul2),
--                          ("div",Binary div2),
--                          ("mod",Binary mod2),
--                          ("neg",Unary neg1),
--                          ("pos",Unary pos1),
--                          ("lt",Binary lt2),
--                          ("lte",Binary lte2),
--                          ("gt",Binary gt2),
--                          ("gte",Binary gte2),
--                          ("eq",Binary eq2),
--                          ("neq",Binary neq2),
--                          ("not",Unary not1),
--                          ("or",Binary or2),
--                          ("and",Binary and2),
--                          ("xor",Binary xor2),
--                          ("nor",Binary nor2),
--                          ("nand",Binary nand2),
--                          ("xnor",Binary xnor2),
--                          ("bwor",Binary bwor2),
--                          ("bwand",Binary bwnor2),
--                          ("bwxor",Binary bwxor2),
--                          ("bwnor",Binary bwnor2),
--                          ("bwnand",Binary bwnand2),
--                          ("bwxnor",Binary bwxnor2),
--                          ("ror",Unary ror1),
--                          ("rand",Unary rand1),
--                          ("rxor",Unary rxor1),
--                          ("rnor",Unary rnor1),
--                          ("rnand",Unary rnand),
--                          ("rxnor",Unary rxnor),
--                          ("shl",Binary shl2),
--                          ("shr",Binary shr2),
--                          ("size",Unary size1),
--                          ("cat",VarArg catx),
--                          ("dup",Binary dup2),
--                          ("fill",Binary fill2),
--                          ("index",Binary index2)
--                          ("range",Trinary range3)]
