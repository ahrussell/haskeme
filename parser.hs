import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Complex
import Data.Ratio
import Data.Array
import Control.Monad.Error

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

instance Show LispVal where 
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "("++(unwordsList contents) ++")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

instance Show LispError where 
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++ show expected 
                                      ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                           ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))]

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ [] = throwError $ Default "Expects one argument"
unaryOp _ multipleVals@(_:_:_) = throwError $ NumArgs 1 multipleVals
unaryOp f [v] = return $ f v

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr
    where unpackStr (String s) = return s
          unpackStr (Number s) = return $ show s
          unpackStr (Bool s) = return $ show s
          unpackStr notString = throwError $ TypeMismatch "string" notString

boolBoolBinop = boolBinop unpackBool
    where unpackBool (Bool b) = return b
          unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                        if null parsed 
                         then throwError $ TypeMismatch "number" $ String n
                         else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

symbolp (Atom _) = Bool True
symbolp _ = Bool False

stringp (String _) = Bool True
stringp _ = Bool False

numberp (Number _) = Bool True
numberp _ = Bool False

boolp (Bool _) = Bool True
boolp _ = Bool False

listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False

symbol2string (Atom s) = String s
string2symbol (String s) = Atom s

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> parseNumber
        <|> parseBool
        <|> parseCharLiteral
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnquoted
        <|> try (do string "#("
                    x <- parseVector
                    char ')'
                    return x)        
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseVector :: Parser LispVal
parseVector = do arrayValues <- sepBy parseExpr spaces
                 return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)

parseQuoted :: Parser LispVal
parseQuoted = parseSugar '\'' "quote"

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = parseSugar '`' "quasiquote"

parseUnquoted :: Parser LispVal
parseUnquoted = parseSugar ',' "unquote"

parseSugar :: Char -> String -> Parser LispVal
parseSugar q r = do char q
                    x <- parseExpr
                    return $ List [Atom r, x] 

parseBool :: Parser LispVal
parseBool = do
                char '#'
                (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many parseChar
                char '"'
                return $ String x

parseChar :: Parser Char
parseChar = (char '\\' >> (oneOf "nrt\\\""))
               <|> noneOf "\""

parseCharLiteral :: Parser LispVal
parseCharLiteral = do
            try $ string "#\\"
            value <- try (string "newline" <|> string "space") 
                 <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
            return $ Character $ case value of
                "space" -> ' '
                "newline" -> '\n'
                otherwise -> (value !! 0)

parseAtom :: Parser LispVal
parseAtom = do 
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ Atom atom

parseFloat :: Parser LispVal
parseFloat = do
                x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat (x++"."++y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal)
                  char '+' 
                  y <- (try parseFloat <|> parseDecimal)
                  char 'i' 
                  return $ Complex (toDouble x :+ toDouble y)
            where toDouble (Float f) = f
                  toDouble (Number n) = fromIntegral n

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseHex <|> parseOct <|> parseBin

parseDecimal :: Parser LispVal
parseDecimal = parseDecimal1 <|> parseDecimal2
        where
            parseDecimal1 = many1 digit >>= (return . Number . read)
            parseDecimal2 = do try $ string "#d"
                               x <- many1 digit
                               (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              h <- many1 hexDigit
              (return . Number . hex2dec) h
      where hex2dec h = fst $ readHex h !! 0

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              o <- many1 octDigit
              (return . Number . oct2dec) o
      where oct2dec o = fst $ readOct o !! 0

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              b <- many1 digit
              (return . Number . bin2dec) b
      where bin2dec = bin2dec' 0
            bin2dec' dec "" = dec
            bin2dec' dec (x:xs) = let acc = 2 * dec + (if x == '0' then 0 else 1)
                                  in bin2dec' acc xs
