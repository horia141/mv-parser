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
          ioList <- enclose squares modIo
          instList <- enclose braces modInst

          return (Mod name attrList inList outList ioList instList False)

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

indent :: Int -> String -> String
indent tab source = 
    intercalate "\n" $ map ((replicate tab ' ')++) $ lines source

data FuncType
    = Unary (String -> String)
    | Binary (String -> String -> String)
    | Ternary (String -> String -> String -> String)
    | VarArg ([String] -> String)


generateExpr :: MVExpr -> [MVDefs] -> String
generateExpr (Func name argList) (toplevel) =
    let argResult = map (flip generateExpr $ toplevel) argList
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
         _ -> error ("The function " ++ name ++ " does not exist!")
    where funTable :: [(String,FuncType)]
          funTable = [("add",    VarArg  (genOperV "+")),
                      ("sub",    VarArg  (genOperV "-")),
                      ("mul",    VarArg  (genOperV "*")),
                      ("div",    VarArg  (genOperV "/")),
                      ("mod",    VarArg  (genOperV "%")),
                      ("neg",    Unary   (genOperU "+")),
                      ("pos",    Unary   (genOperU "-")),
                      ("lt",     Binary  (genOperB "<")),
                      ("lte",    Binary  (genOperB "<=")),
                      ("gt",     Binary  (genOperB ">")),
                      ("gte",    Binary  (genOperB ">=")),
                      ("eq",     Binary  (genOperB "==")),
                      ("neq",    Binary  (genOperB "!=")),
                      ("not",    Unary   (genOperU "!")),
                      ("or",     VarArg  (genOperV "||")),
                      ("and",    VarArg  (genOperV "&&")),
                      ("xor",    VarArg  (genXor)),
                      ("nor",    VarArg  (genOperU "!" . genOperV "||")),
                      ("nand",   VarArg  (genOperU "!" . genOperV "&&")),
                      ("xnor",   VarArg  (genOperU "!" . genXor)),
                      ("bwnot",  Unary   (genOperU "~")),
                      ("bwor",   VarArg  (genOperV "|")),
                      ("bwand",  VarArg  (genOperV "&")),
                      ("bwxor",  VarArg  (genOperV "^")),
                      ("bwnor",  VarArg  (genOperU "~" . genOperV "|")),
                      ("bwnand", VarArg  (genOperU "~" . genOperV "&")),
                      ("bwxnor", VarArg  (genOperV "~^")),
                      ("shl",    Binary  (genOperB "<<")),
                      ("shr",    Binary  (genOperB ">>")),
                      ("cat",    VarArg  (genEnclose "{" "}" . genOperV ",")),
                      ("index",  Binary  (genIndex)),
                      ("range",  Ternary (genRange))]

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
          
generateExpr (Path inst name) (toplevel) =
    inst ++ "_" ++ name
generateExpr (Symb name) (toplevel) =
    case (find testName toplevel) of
      Just (Def defName defValue defBuiltin) -> generateExpr defValue toplevel
      _ -> error ("Can't find " ++ name)
    where testName :: MVDefs -> Bool
          testName (Def testName testValue testBuiltin) =
              testName == name
          testName _ = False

generateExpr (Numb value) (toplevel) =
    "'b" ++ value

generateDefs :: MVDefs -> [MVDefs] -> Int -> String
generateDefs (Def name value builtin) (toplevel) (tab) = 
    indent tab $ "`define " ++ name ++ " " ++ (generateExpr value toplevel)
generateDefs (Mod name attrList inList outList ioList instList builtin) (toplevel) (tab) =
    indent tab $ "module " ++ name ++ "();" ++ "\n" ++ "endmodule"
generateDefs (Fsm name attrList inList outList stateList builtin) (toplevel) (tab) =
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
                        indent 2 $ "parameter " ++ name ++ " = 0;"

          genIns :: String
          genIns = unlines $ map genIn inList
              where genIn :: MVUnitFsmIn -> String
                    genIn (FsmIn name size) =
                        indent 2 $ "input wire[0:" ++ (generateExpr size toplevel) ++ "-1] " ++ name ++ ";"

          genOuts :: String
          genOuts = unlines $ map genOut outList
              where genOut :: MVUnitFsmOut -> String
                    genOut (FsmOut name size offset) = 
                        indent 2 $ "output wire[0:" ++ (generateExpr size toplevel) ++ "-1] " ++ name ++ ";\n" ++
                                   "assign " ++ name ++ " = fsm_output[" ++ (generateExpr offset toplevel) ++ ":" ++ (generateExpr size toplevel) ++ "];//wrong"
          
compile :: FilePath -> IO String
compile path
    = do fileContents <- readFile path
         
         case parse (lexparser (many defs)) "" fileContents of
           Left err -> do putStr "syntax error at "
                          return (show err)
           Right res -> return $ intercalate "\n\n" (map (\ x -> generateDefs x res 0) res)
    where lexparser parser = do whiteSpace lexer
                                res <- parser
                                eof
                                return res

