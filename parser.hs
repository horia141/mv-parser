module Main where

import Data.List
import Data.Bits
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Numeric

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import System.Environment
import System.Console.GetOpt

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
          x2b 'b' input = input
          x2b 'd' input = 
              let intInput = read input :: Integer
              in case intInput of
                   0 -> "0"
                   _ -> reverse $ doRec intInput
                  where doRec :: Integer -> String
                        doRec 0 = ""
                        doRec num = 
                            case num .&. 1 of
                              0 -> '0':(doRec $ shiftR num 1)
                              1 -> '1':(doRec $ shiftR num 1)
          x2b 'o' input = foldl (++) [] $ map (o2b_dig) input
              where o2b_dig 'z' = "zzz";  o2b_dig 'Z' = "zzz";  o2b_dig 'x' = "xxx";  o2b_dig 'X' = "xxx"
                    o2b_dig '0' = "000";  o2b_dig '1' = "001";  o2b_dig '2' = "010";  o2b_dig '3' = "011"
                    o2b_dig '4' = "100";  o2b_dig '5' = "101";  o2b_dig '6' = "110";  o2b_dig '7' = "111"
          x2b 'h' input = foldl (++) [] $ map (h2b_dig) input
              where h2b_dig 'z' = "zzzz"; h2b_dig 'Z' = "zzzz"; h2b_dig 'x' = "xxxx"; h2b_dig 'X' = "xxxx"
                    h2b_dig '0' = "0000"; h2b_dig '1' = "0001"; h2b_dig '2' = "0010"; h2b_dig '3' = "0011"
                    h2b_dig '4' = "0100"; h2b_dig '5' = "0101"; h2b_dig '6' = "0110"; h2b_dig '7' = "0111"
                    h2b_dig '8' = "1000"; h2b_dig '9' = "1001"; h2b_dig 'a' = "1010"; h2b_dig 'A' = "1010"
                    h2b_dig 'b' = "1011"; h2b_dig 'B' = "1011"; h2b_dig 'c' = "1100"; h2b_dig 'C' = "1100"
                    h2b_dig 'd' = "1101"; h2b_dig 'D' = "1101"; h2b_dig 'e' = "1110"; h2b_dig 'E' = "1110"
                    h2b_dig 'f' = "1111"; h2b_dig 'F' = "1111"

defs :: Parser MVDefs
defs = ddef <|> dmod <|> dfsm

ddef :: Parser MVDefs
ddef = do reserved lexer "def"
          name <- identifier lexer
          value <- expr

          return (Def name value False)

dmod :: Parser MVDefs
dmod = do reserved lexer "mod"
          name <- identifier lexer
          attrList <- enclose squares modAttr
          inList <- enclose squares modIn
          outList <- enclose squares modOut
          instList <- enclose braces modInst

          return (Mod name attrList inList outList instList False)

dfsm :: Parser MVDefs
dfsm = do reserved lexer "fsm"
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

indent :: Int -> String -> String
indent tab source = 
    intercalate "\n" $ map ((replicate tab ' ')++) $ lines source

data FuncType
    = Constant (String)
    | Unary (String -> String)
    | Binary (String -> String -> String)
    | Ternary (String -> String -> String -> String)
    | VarArg ([String] -> String)


generateExpr :: MVExpr -> Map String MVDefs -> String
generateExpr (Func name argList) (toplevel) =
    let argResult = map (flip generateExpr $ toplevel) argList
    in case find (\ (x,_) -> name == x) funTable of
         Just (name,Constant func) -> if (length argResult) == 0
                                      then func
                                      else error ("Invalid number of arguments for function " ++ name ++ "!")
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
         _ -> error ("The function " ++ name ++ " does not exist!")
    where funTable :: [(String,FuncType)]
          funTable = [("add",    VarArg   (genOperV "+")),
                      ("sub",    VarArg   (genOperV "-")),
                      ("mul",    VarArg   (genOperV "*")),
                      ("div",    VarArg   (genOperV "/")),
                      ("mod",    VarArg   (genOperV "%")),
                      ("neg",    Unary    (genOperU "+")),
                      ("pos",    Unary    (genOperU "-")),
                      ("lt",     Binary   (genOperB "<")),
                      ("lte",    Binary   (genOperB "<=")),
                      ("gt",     Binary   (genOperB ">")),
                      ("gte",    Binary   (genOperB ">=")),
                      ("eq",     Binary   (genOperB "==")),
                      ("neq",    Binary   (genOperB "!=")),
                      ("not",    Unary    (genOperU "!")),
                      ("or",     VarArg   (genOperV "||")),
                      ("and",    VarArg   (genOperV "&&")),
                      ("xor",    VarArg   (genXor)),
                      ("nor",    VarArg   (genOperU "!" . genOperV "||")),
                      ("nand",   VarArg   (genOperU "!" . genOperV "&&")),
                      ("xnor",   VarArg   (genOperU "!" . genXor)),
                      ("bwnot",  Unary    (genOperU "~")),
                      ("bwor",   VarArg   (genOperV "|")),
                      ("bwand",  VarArg   (genOperV "&")),
                      ("bwxor",  VarArg   (genOperV "^")),
                      ("bwnor",  VarArg   (genOperU "~" . genOperV "|")),
                      ("bwnand", VarArg   (genOperU "~" . genOperV "&")),
                      ("bwxnor", VarArg   (genOperV "~^")),
                      ("shl",    Binary   (genOperB "<<")),
                      ("shr",    Binary   (genOperB ">>")),
                      ("cat",    VarArg   (genEnclose "{" "}" . genOperV ",")),
                      ("index",  Binary   (genIndex)),
                      ("range",  Ternary  (genRange)),
                      ("else",   Constant (genElse))]

          genOperU :: String -> String -> String
          genOperU oper arg =
              oper ++ " " ++ (genParens arg)

          genOperB :: String -> String -> String -> String
          genOperB oper arg0 arg1 =
              (genParens arg0) ++ " " ++ oper ++ " " ++ (genParens arg1)

          genOperV :: String -> [String] -> String
          genOperV oper argList =
              intercalate (" " ++ oper ++ " ") $ map genParens argList

          genEnclose :: String -> String -> String -> String
          genEnclose parens0 parens1 arg =
              parens0 ++ arg ++ parens1

          genParens :: String -> String
          genParens arg =
              genEnclose "(" ")" arg

          genXor :: [String] -> String
          genXor argList = 
              foldl genXor2 (head argList) (tail argList)
              where genXor2 :: String -> String -> String
                    genXor2 arg0 arg1 =
                        let arg0p = genParens arg0
                            arg1p = genParens arg1
                        in 
                          (genParens (arg0p ++ " && " ++ (genParens ("!" ++ arg1p)))) ++ " || " ++ (genParens ((genParens ("!" ++ arg0p)) ++ " && " ++ arg1p))

          genIndex :: String -> String -> String
          genIndex arg index =
              (genParens arg) ++ "[" ++ index ++ "]"

          genRange :: String -> String -> String -> String
          genRange arg beg end =
              (genParens arg) ++ "[" ++ beg ++ ":" ++ end ++ "]"

          genElse :: String
          genElse = "1"
generateExpr (Path inst name) (toplevel) =
    inst ++ "_" ++ name
generateExpr (Symb name) (toplevel) =
    case name `Map.lookup` toplevel of
      Just (Def defName defValue defBuiltin) -> generateExpr defValue toplevel
      _ -> name
generateExpr (Numb value) (toplevel) =
    "'b" ++ value

generateDefs :: MVDefs -> Map String MVDefs -> Int -> String
generateDefs (Def name value builtin) (toplevel) (tab) = 
    indent tab $ "`define " ++ name ++ " " ++ (generateExpr value toplevel)
generateDefs (Mod name attrList inList outList instList builtin) (toplevel) (tab) =
    indent tab ("module " ++ name ++ "(" ++ genParamList ++ ");" ++ "\n" ++ 
                genAttrs ++ "\n" ++
                genIns ++ "\n" ++
                genOuts ++ "\n" ++
                genInsts ++ "\n" ++
                "endmodule // " ++ name)
    where genParamList :: String
          genParamList = intercalate "," ((map modInName inList) ++ (map modOutName outList))

          genAttrs :: String
          genAttrs = unlines $ map genAttr attrList
              where genAttr :: MVUnitModAttr -> String
                    genAttr (ModAttr name) =
                        indent 2 $ "parameter " ++ name ++ " = 8;"

          genIns :: String
          genIns = unlines $ map genIn inList
              where genIn :: MVUnitModIn -> String
                    genIn (ModIn name size) =
                        indent 2 $ "input wire [" ++ (generateExpr (Func "sub" [size, (Numb "1")]) toplevel) ++ ":0] " ++ name ++ ";"

          genOuts :: String
          genOuts = unlines $ map genOut outList
              where genOut :: MVUnitModOut -> String
                    genOut (ModOut name size sink) = 
                        indent 2 $ "output wire [" ++ (generateExpr (Func "sub" [size, (Numb "1")]) toplevel) ++ ":0] " ++ name ++ ";" ++ "\n" ++
                                   "assign " ++ name ++ " = " ++ (generateExpr sink toplevel) ++ ";"

          genInsts :: String
          genInsts = unlines $ map genInst instList
              where genInst :: MVUnitModInst -> String
                    genInst (ModInst kind name attrList nvPairList) =
                        case Map.lookup kind toplevel of
                             Just (Mod modName modAttrList modInList modOutList _ _) -> 
                                 indent 2 ((genWireModOutAttrsHack modAttrList) ++ "\n" ++ 
                                           (genWireModOut modOutList modAttrList) ++ "\n" ++ 
                                           kind ++ " #(" ++ "\n" ++ 
                                           (indent 4 $ (genInstModAttr modAttrList) ++ ")") ++ "\n" ++
                                           (indent 2 $ name ++ "(" ++ "\n" ++ 
                                                       (indent 2 $ genInstModIn modInList) ++ ",\n" ++ 
                                                       (indent 2 $ genInstModOut modOutList) ++ ");"))
                             Just (Fsm fsmName fsmAttrList fsmInList fsmOutList _ _) -> 
                                 indent 2 ((genWireFsmOutAttrsHack fsmAttrList) ++ "\n" ++ 
                                           (genWireFsmOut fsmOutList fsmAttrList) ++ "\n" ++
                                           kind ++ " #(" ++ "\n" ++ 
                                           (indent 4 $ (genInstFsmAttr fsmAttrList) ++ ")") ++ "\n" ++
                                           (indent 2 $ name ++ "(" ++ "\n" ++
                                                       (indent 2 $ genInstFsmIn fsmInList) ++ ",\n" ++ 
                                                       (indent 2 $ genInstFsmOut fsmOutList) ++ ");"))
                             _ -> error ("Unknown module " ++ kind ++ "!")
                        where genInstModAttr :: [MVUnitModAttr] -> String
                              genInstModAttr (modAttrList) = 
                                  if (length modAttrList) == (length attrList)
                                  then intercalate ",\n" $ zipWith genAttrExpr modAttrList attrList
                                  else error ("Invalid attribute list for " ++ kind ++ " " ++ name)
                                  where genAttrExpr :: MVUnitModAttr -> MVExpr -> String
                                        genAttrExpr (ModAttr name) (expr) =
                                            "." ++ name ++ "(" ++ (generateExpr expr toplevel) ++ ")"

                              genInstModIn :: [MVUnitModIn] -> String
                              genInstModIn (modInList) = 
                                  if (length modInList) == (length nvPairList)
                                  then intercalate ",\n" $ map genInExpr nvPairList
                                  else error ("Invalid input list for " ++ kind ++ " " ++ name)
                                  where genInExpr :: MVUnitModNVPair -> String
                                        genInExpr (ModNVPair name value) =
                                            case find testIn modInList of
                                              Just _  -> "." ++ name ++ "(" ++ (generateExpr value toplevel) ++ ")"
                                              Nothing -> error (kind ++ " " ++ name ++ " does not have an input named " ++ name ++ "!")
                                            where testIn :: MVUnitModIn -> Bool
                                                  testIn (ModIn inName _) = name == inName

                              genInstModOut :: [MVUnitModOut] -> String
                              genInstModOut (modOutList) = intercalate ",\n" $ map genOut modOutList
                                      where genOut :: MVUnitModOut -> String
                                            genOut (ModOut outName outSize outSink) = 
                                                "." ++ outName ++ "(" ++ name ++ "_" ++ outName ++ ")"

                              genWireModOutAttrsHack :: [MVUnitModAttr] -> String
                              genWireModOutAttrsHack (modAttrList) = unlines $ zipWith genWireOutAttr modAttrList attrList
                                  where genWireOutAttr (ModAttr attrName) (expr) = 
                                            "parameter hack_" ++ name ++ "_" ++ attrName ++ " = " ++ (generateExpr expr toplevel) ++ ";"

                              genWireModOut :: [MVUnitModOut] -> [MVUnitModAttr] -> String
                              genWireModOut (modOutList) (modAttrList) = unlines $ map genWireOut modOutList
                                  where genWireOut :: MVUnitModOut -> String
                                        genWireOut (ModOut outName outSize outSink) = 
                                            "wire [" ++ (generateExpr (Func "sub" [(hackizeOutput outSize), (Numb "1")]) toplevel) ++ ":0] " ++ name ++ "_" ++ outName ++ ";"

                                        hackizeOutput :: MVExpr -> MVExpr
                                        hackizeOutput (Func funcName argList) =
                                            (Func funcName (map hackizeOutput argList))
                                        hackizeOutput (Symb symbName) = 
                                            (Symb ("hack_" ++ name ++ "_" ++ symbName))
                                        hackizeOutput (expr) = expr

                              genInstFsmAttr :: [MVUnitFsmAttr] -> String
                              genInstFsmAttr (fsmAttrList) = 
                                  if (length fsmAttrList) == (length attrList)
                                  then intercalate ",\n" $ zipWith genAttrExpr fsmAttrList attrList
                                  else error ("Invalid attribute list for " ++ kind ++ " " ++ name)
                                  where genAttrExpr (FsmAttr name) (expr) = 
                                            "." ++ name ++ "(" ++ (generateExpr expr toplevel) ++ ")"

                              genInstFsmIn :: [MVUnitFsmIn] -> String
                              genInstFsmIn (fsmInList) = 
                                  if (length fsmInList) == (length nvPairList)
                                  then intercalate ",\n" $ map genInExpr nvPairList
                                  else error ("Invalid input list for " ++ kind ++ " " ++ name)
                                  where genInExpr :: MVUnitModNVPair -> String
                                        genInExpr (ModNVPair name value) = 
                                            case find testIn fsmInList of
                                              Just _  -> "." ++ name ++ "(" ++ (generateExpr value toplevel) ++ ")"
                                              Nothing -> error (kind ++ " " ++ name ++ " does not have an input named " ++ name ++ "!")
                                            where testIn :: MVUnitFsmIn -> Bool
                                                  testIn (FsmIn inName _) = name == inName

                              genInstFsmOut :: [MVUnitFsmOut] -> String
                              genInstFsmOut (fsmOutList) = intercalate ",\n" $ map genOut fsmOutList
                                  where genOut :: MVUnitFsmOut -> String
                                        genOut (FsmOut outName outSize outSink) =
                                            "." ++ outName ++ "(" ++ name ++ "_" ++ outName ++ ")"

                              genWireFsmOutAttrsHack :: [MVUnitFsmAttr] -> String
                              genWireFsmOutAttrsHack (fsmAttrList) = unlines $ zipWith genWireOutAttr fsmAttrList attrList
                                  where genWireOutAttr (FsmAttr attrName) (expr) =
                                            "parameter hack_" ++ name ++ "_" ++ attrName ++ " = " ++ (generateExpr expr toplevel) ++ ";"

                              genWireFsmOut :: [MVUnitFsmOut] -> [MVUnitFsmAttr] -> String
                              genWireFsmOut (fsmOutList) (fsmAttrList) = unlines $ map genWireOut fsmOutList
                                  where genWireOut :: MVUnitFsmOut -> String
                                        genWireOut (FsmOut outName outSize outOffset) =
                                            "wire [" ++ (generateExpr (Func "sub" [(hackizeOutput outSize), (Numb "1")]) toplevel) ++ ":0] " ++ name ++ "_" ++ outName ++ ";"

                                        hackizeOutput :: MVExpr -> MVExpr
                                        hackizeOutput (Func funcName argList) =
                                            (Func funcName (map hackizeOutput argList))
                                        hackizeOutput (Symb symbName) = 
                                            (Symb ("hack_" ++ name ++ "_" ++ symbName))
                                        hackizeOutput (expr) = expr
generateDefs (Fsm name attrList inList outList stateList builtin) (toplevel) (tab) =
    let states     = zip (map fsmStateName stateList) $ zip3 (map ((name++) . ("_"++) . fsmStateName) stateList) (map fsmStateOutput stateList) [0..(genericLength stateList)-1]
        stateBits  = if (length stateList) > 1
                     then ceiling $ log (genericLength stateList) / log 2
                     else 1
        outputBits = if not $ null stateList
                     then genericLength $ numbValue $ fsmStateOutput $ head stateList
                     else 1
    in indent tab ("module " ++ name ++ "(" ++ genParamList ++ ");" ++ "\n" ++
                   genAttrs ++ "\n" ++ 
                   genIns ++ "\n" ++ 
                   genOuts ++ "\n" ++
                   (genInternalRegs stateBits outputBits) ++ "\n" ++
                   (genStateDefines states) ++ "\n\n" ++
                   (genStateDefinesText states) ++ "\n\n" ++
                   (genStateChanges states) ++ "\n\n" ++
                   (genStateOutputs states) ++ "\n" ++
                   "endmodule // " ++ name)
    where genParamList :: String
          genParamList = intercalate "," ((map fsmInName inList) ++ (map fsmOutName outList))

          genAttrs :: String
          genAttrs = unlines $ map genAttr attrList
              where genAttr :: MVUnitFsmAttr -> String
                    genAttr (FsmAttr name) =
                        indent 2 $ "parameter " ++ name ++ " = 8;"

          genIns :: String
          genIns = unlines $ map genIn inList
              where genIn :: MVUnitFsmIn -> String
                    genIn (FsmIn name size) =
                        indent 2 $ "input wire[" ++ (generateExpr (Func "sub" [size, (Numb "1")]) toplevel) ++ ":0] " ++ name ++ ";"

          genOuts :: String
          genOuts = unlines $ map genOut outList
              where genOut :: MVUnitFsmOut -> String
                    genOut (FsmOut name size offset) = 
                        indent 2 $ "output wire[" ++ (generateExpr (Func "sub" [size, (Numb "1")]) toplevel) ++ ":0] " ++ name ++ ";\n" ++
                                   "assign " ++ name ++ " = fsm_output[" ++ (generateExpr (Func "sub" [(Func "add" [offset,size]), (Numb "1")]) toplevel) ++ ":" ++ (generateExpr offset toplevel) ++ "];"

          genInternalRegs :: Integer -> Integer -> String
          genInternalRegs stateBits outputBits = 
              unlines [indent 2 $ "reg[" ++ (show stateBits) ++ "-1:0] fsm_state;",
                       indent 2 $ "reg[64 * 8 - 1:0] fsm_state_t;",
                       indent 2 $ "reg[" ++ (show outputBits) ++ "-1:0] fsm_output;"]

          genStateDefines :: [(String,(String,MVExpr,Integer))] -> String
          genStateDefines (states) = indent 2 $ unlines $ map genStateDefine states
              where genStateDefine :: (String,(String,MVExpr,Integer)) -> String
                    genStateDefine (stateName,(fullName,output,code)) =
                        "`define " ++ fullName ++ " " ++ (show code)

          genStateDefinesText :: [(String,(String,MVExpr,Integer))] -> String
          genStateDefinesText (states) = indent 2 $ unlines $ map genStateDefineText states
              where genStateDefineText :: (String,(String,MVExpr,Integer)) -> String
                    genStateDefineText (stateName,(fullName,output,code)) = 
                        "`define " ++ fullName ++ "_t \"" ++ stateName ++ "\""

          genStateChanges :: [(String,(String,MVExpr,Integer))] -> String
          genStateChanges states = 
              let statesMap = Map.fromList states
              in indent 2 $ "always @ (posedge clock) begin" ++ "\n" ++
                     (indent 2 $ "if (reset) begin") ++ "\n" ++ 
                     (indent 4 $ "fsm_state <= `" ++ (let (fullName,_,_) = (statesMap Map.! "Reset") in fullName) ++ ";") ++ "\n" ++
                     (indent 2 $ "end") ++ "\n" ++
                     (indent 2 $ "else begin") ++ "\n" ++
                     (indent 4 $ "case (fsm_state)") ++ "\n" ++
                     (indent 4 $ unlines $ map (genStateChange statesMap) stateList) ++ "\n" ++
                     (indent 4 $ "endcase") ++ "\n" ++
                     (indent 2 $ "end") ++ "\n" ++
                     "end"
              where genStateChange :: Map String (String,MVExpr,Integer) -> MVUnitFsmState -> String
                    genStateChange (statesMap) (FsmState name output changeList) = 
                        let (fullName,_,_) = statesMap Map.! name
                        in indent 2 $ "`" ++ fullName ++ ":begin" ++ "\n" ++
                           (indent 2 $ genChangeList changeList) ++ "\n" ++
                           "end"
                        where genChangeList :: [MVUnitFsmChange] -> String
                              genChangeList ((FsmChange state (Func "else" [])):[]) =
                                  let (stateFullName,_,_) = statesMap Map.! state
                                  in "fsm_state <= `" ++ stateFullName ++ ";" ++ "\n" ++
                                     "fsm_state_t <= `" ++ stateFullName ++ "_t;"
                              genChangeList ((FsmChange state expr):restChangeList) =
                                  let (stateFullName,_,_) = statesMap Map.! state
                                  in "if (" ++ (generateExpr expr toplevel) ++ ") begin" ++ "\n" ++
                                     (indent 2 $ "fsm_state <= `" ++ stateFullName ++ ";" ++ "\n" ++
                                                 "fsm_state_t <= `" ++ stateFullName ++ "_t;") ++ "\n" ++
                                     "end" ++ "\n" ++ (unlines $ map genChange restChangeList)
                                  where genChange :: MVUnitFsmChange -> String
                                        genChange (FsmChange state (Func "else" [])) =
                                            let (stateFullName,_,_) = statesMap Map.! state
                                            in "else begin" ++ "\n" ++
                                               (indent 2 $ "fsm_state <= `" ++ stateFullName ++ ";" ++ "\n" ++
                                                           "fsm_state_t <= `" ++ stateFullName ++ "_t;") ++ "\n" ++
                                               "end"
                                        genChange (FsmChange state expr) = 
                                            let (stateFullName,_,_) = statesMap Map.! state
                                            in "else" ++ "\n" ++
                                               "if (" ++ (generateExpr expr toplevel) ++ ") begin" ++ "\n" ++
                                               (indent 2 $ "fsm_state <= `" ++ stateFullName ++ ";" ++ "\n" ++
                                                           "fsm_state_t <= `" ++ stateFullName ++ "_t;") ++ "\n" ++
                                               "end"

          genStateOutputs :: [(String,(String,MVExpr,Integer))] -> String
          genStateOutputs states = indent 2 $ "always @ (fsm_state) begin" ++ "\n" ++
                                   (indent 2 $ "case (fsm_state)") ++ "\n" ++
                                   (indent 2 $ unlines $ map genStateOutput states) ++ "\n" ++
                                   (indent 2 $ "endcase") ++ "\n" ++
                                  "end"
              where genStateOutput :: (String,(String,MVExpr,Integer)) -> String
                    genStateOutput (stateName,(fullName,output,code)) =
                        indent 2 $ "`" ++ fullName ++ ": fsm_output = " ++ (generateExpr output toplevel) ++ ";"

toplevel :: Map String MVDefs
toplevel = Map.fromList [("Reg",Mod "Reg"
                                   [ModAttr "Size"]
                                   [ModIn "clock" (Numb "1"),
                                    ModIn "reset" (Numb "1"),
                                    ModIn "data_i" (Symb "Size"),
                                    ModIn "writeEn" (Numb "1")]
                                   [ModOut "data_o" (Symb "Size") (Symb "builtin")]
                                   []
                                   True),
                         ("UpCounter",Mod "UpCounter"
                                         [ModAttr "Size"]
                                         [ModIn "clock" (Numb "1"),
                                          ModIn "reset" (Numb "1"),
                                          ModIn "count" (Numb "1")]
                                         [ModOut "data_o" (Symb "Size") (Symb "builtin")]
                                         []
                                         True),
                         ("UDCounter",Mod "UDCounter"
                                         [ModAttr "Size"]
                                         [ModIn "clock" (Numb "1"),
                                          ModIn "reset" (Numb "1"),
                                          ModIn "count" (Numb "1"),
                                          ModIn "direction" (Numb "1")]
                                         [ModOut "data_o" (Symb "Size") (Symb "builtin")]
                                         []
                                         True),
                         ("Mux2",Mod "Mux2"
                                     [ModAttr "Size"]
                                     [ModIn "select" (Numb "1"),
                                      ModIn "data_i00" (Symb "Size"),
                                      ModIn "data_i01" (Symb "Size")]
                                     [ModOut "data_o" (Symb "Size") (Symb "builtin")]
                                     []
                                     True),
                         ("Mux4",Mod "Mux4"
                                     [ModAttr "Size"]
                                     [ModIn "select" (Numb "2"),
                                      ModIn "data_i00" (Symb "Size"),
                                      ModIn "data_i01" (Symb "Size"),
                                      ModIn "data_i02" (Symb "Size"),
                                      ModIn "data_i03" (Symb "Size")]
                                     [ModOut "data_o" (Symb "Size") (Symb "builtin")]
                                     []
                                     True),
                         ("Mux8",Mod "Mux8"
                                     [ModAttr "Size"]
                                     [ModIn "select" (Numb "3"),
                                      ModIn "data_i00" (Symb "Size"),
                                      ModIn "data_i01" (Symb "Size"),
                                      ModIn "data_i02" (Symb "Size"),
                                      ModIn "data_i03" (Symb "Size"),
                                      ModIn "data_i04" (Symb "Size"),
                                      ModIn "data_i05" (Symb "Size"),
                                      ModIn "data_i06" (Symb "Size"),
                                      ModIn "data_i07" (Symb "Size")]
                                     [ModOut "data_o" (Symb "Size") (Symb "builtin")]
                                     []
                                     True),
                         ("Mux16",Mod "Mux16"
                                     [ModAttr "Size"]
                                     [ModIn "select" (Numb "4"),
                                      ModIn "data_i00" (Symb "Size"),
                                      ModIn "data_i01" (Symb "Size"),
                                      ModIn "data_i02" (Symb "Size"),
                                      ModIn "data_i03" (Symb "Size"),
                                      ModIn "data_i04" (Symb "Size"),
                                      ModIn "data_i05" (Symb "Size"),
                                      ModIn "data_i06" (Symb "Size"),
                                      ModIn "data_i07" (Symb "Size"),
                                      ModIn "data_i08" (Symb "Size"),
                                      ModIn "data_i09" (Symb "Size"),
                                      ModIn "data_i10" (Symb "Size"),
                                      ModIn "data_i11" (Symb "Size"),
                                      ModIn "data_i12" (Symb "Size"),
                                      ModIn "data_i13" (Symb "Size"),
                                      ModIn "data_i14" (Symb "Size"),
                                      ModIn "data_i15" (Symb "Size")]
                                     [ModOut "data_o" (Symb "Size") (Symb "builtin")]
                                     []
                                     True),
                         ("Mux32",Mod "Mux32"
                                     [ModAttr "Size"]
                                     [ModIn "select" (Numb "5"),
                                      ModIn "data_i00" (Symb "Size"),
                                      ModIn "data_i01" (Symb "Size"),
                                      ModIn "data_i02" (Symb "Size"),
                                      ModIn "data_i03" (Symb "Size"),
                                      ModIn "data_i04" (Symb "Size"),
                                      ModIn "data_i05" (Symb "Size"),
                                      ModIn "data_i06" (Symb "Size"),
                                      ModIn "data_i07" (Symb "Size"),
                                      ModIn "data_i08" (Symb "Size"),
                                      ModIn "data_i09" (Symb "Size"),
                                      ModIn "data_i10" (Symb "Size"),
                                      ModIn "data_i11" (Symb "Size"),
                                      ModIn "data_i12" (Symb "Size"),
                                      ModIn "data_i13" (Symb "Size"),
                                      ModIn "data_i14" (Symb "Size"),
                                      ModIn "data_i15" (Symb "Size"),
                                      ModIn "data_i16" (Symb "Size"),
                                      ModIn "data_i17" (Symb "Size"),
                                      ModIn "data_i18" (Symb "Size"),
                                      ModIn "data_i19" (Symb "Size"),
                                      ModIn "data_i20" (Symb "Size"),
                                      ModIn "data_i21" (Symb "Size"),
                                      ModIn "data_i22" (Symb "Size"),
                                      ModIn "data_i23" (Symb "Size"),
                                      ModIn "data_i24" (Symb "Size"),
                                      ModIn "data_i25" (Symb "Size"),
                                      ModIn "data_i26" (Symb "Size"),
                                      ModIn "data_i27" (Symb "Size"),
                                      ModIn "data_i28" (Symb "Size"),
                                      ModIn "data_i29" (Symb "Size"),
                                      ModIn "data_i30" (Symb "Size"),
                                      ModIn "data_i31" (Symb "Size")]
                                     [ModOut "data_o" (Symb "Size") (Symb "builtin")]
                                     []
                                     True)]

compile :: String -> IO String
compile text =
    case parse (lexparser (many defs)) "" text of
      Left err -> do putStr "syntax error at "
                     putStr (show err)
                     ioError $ userError "Aborting ..."
      Right res -> return $ let toplevel' = Map.union toplevel $ Map.fromList $ zip (map defsName res) res
                            in intercalate "\n\n" (map (\ x -> generateDefs x toplevel' 0) res)
    where lexparser parser = do whiteSpace lexer
                                res <- parser
                                eof
                                return res

          defsName :: MVDefs -> String
          defsName (Def name _ _) = name
          defsName (Mod name _ _ _ _ _) = name
          defsName (Fsm name _ _ _ _ _) = name

main :: IO ()
main =
    do argv <- getArgs

       case clineOpts argv of
         Left ([],nonOptArgs) -> 
             do (vFiles,mvFiles) <- assembleOutput nonOptArgs
                mvCompiledFiles <- mapM compile mvFiles
                
                putStrLn $  (concat vFiles) ++ (concat mvCompiledFiles)
         Left (Output outputPath:[],nonOptArgs) ->
             do (vFiles,mvFiles) <- assembleOutput nonOptArgs
                mvCompiledFiles <- mapM compile mvFiles

                writeFile outputPath $ (concat vFiles) ++ (concat mvCompiledFiles) ++ "\n"
         Right errorMessages -> 
             ioError $ userError $ concat errorMessages ++ usageInfo clineHeader clineOptionDesc
    where assembleOutput :: [String] -> IO ([String],[String])
          assembleOutput (nonOptArgs) = 
              let vFilePaths = filter (".v" `isSuffixOf`) nonOptArgs
                  mvFilePaths = filter (".mv" `isSuffixOf`) nonOptArgs
                  errFilePaths = filter (\ x -> not (".v" `isSuffixOf` x || ".mv" `isSuffixOf` x)) nonOptArgs
              in do vFiles <- mapM readFile vFilePaths
                    mvFiles <- mapM readFile mvFilePaths

                    case null errFilePaths of
                      True -> return (vFiles,mvFiles)
                      False -> ioError $ userError ("Unrecognized extensions for files: " ++ (intercalate " " errFilePaths))

data CLineOption
    = Output {
        outputPath :: String}
    deriving (Show)

clineOptionDesc :: [OptDescr CLineOption]
clineOptionDesc = [Option ['o'] ["outpath"] (ReqArg Output "FILE") "output FILE"]

clineHeader :: String
clineHeader = "Usage: mvc [OPTIONS...] input_files..."

clineOpts :: [String] -> Either ([CLineOption],[String]) [String]
clineOpts (argv) = 
    case getOpt Permute clineOptionDesc argv of
      (optArgs,nonOptArgs,[]) -> Left (optArgs,nonOptArgs)
      (_,_,errorMessages)     -> Right errorMessages
