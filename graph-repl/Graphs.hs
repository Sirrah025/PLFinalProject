{-# Language GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module Graphs where

import           Prelude
import           Parsing2
import           Codec.Picture
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture

type Graph = Drawing PixelRGBA8 ()

--Here is a data type for individual operations in an expression.
data Op where
  Plus   :: Op
  Minus  :: Op
  Times  :: Op
  Divide :: Op
  Expo   :: Op
  deriving (Show, Eq)

--This is data type to represent constants like pi.
data Const where
  Pi :: Const
  E  :: Const
  deriving (Show, Eq)

--This data represents different parts of an arithematic expression.
data Arith where
  X    :: Arith
  Lit  :: Float -> Arith
  Neg  :: Arith -> Arith
  Bin  :: Op -> Arith -> Arith -> Arith
  Abs  :: Arith -> Arith
  Cons :: Const -> Arith
  deriving (Show)
  
--This data represents various errors that can occur in running the program.
data InterpError where
  DivByZero     :: InterpError
  InvalidValues :: InterpError

--This data represents the bounds of the function.
data Bounds where
  Bound :: Float -> Float -> Bounds
  deriving (Show)
  
--This data represents the arithematic expression and its bounds combined.
data BoundedAriths where
  BoundedArith :: Bounds -> Arith -> BoundedAriths

--This function gives an error message depending on the error given.
showInterpError :: InterpError -> String
showInterpError DivByZero     = "Division by zero"
showInterpError InvalidValues = "Values given are invalid" 

--This function abstracts operators.
operate :: Op -> Float -> Float -> Float 
operate Plus   = (+)
operate Minus  = (-)
operate Times  = (*)
operate Divide = (/)
operate Expo   = (**)

lexer :: TokenParser u
lexer      = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens     = getParens lexer

bracks :: Parser a -> Parser a
bracks     = getBrackets lexer

symbol :: String -> Parser String
symbol     = getSymbol lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved   = getReserved lexer

float :: Parser Double
float      = getFloat lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseNaturalOrDouble :: Parser (Either Integer Double)
parseNaturalOrDouble = getNaturalOrFloat lexer

description :: String
description = "Welcome! Type in an expression. Type :help if you need more information."

helpMsg :: String
helpMsg = unlines
  [ "Enter an expression to generate a 256x256 image and save it to graph.png."
  , "Make sure to enter the bounds you would like to calculate the expression in."
  , "Example: [0,10]:x+1 calculates the function x+1 between the points 0 and 10."
  , ":size              - report the current size used by the :save command."
  , ":size <int> <int>  - set the size used by the :save command."
  , ":save <file>       - save a :size x :size copy of the most recent expression to <file>.png and <file>.txt"
  , ":help              - print this message."
  , ":quit              - quit."
  , ""
  , "Your command history is automatically saved in a file named graph_history.txt."
  ]

--parses individual expressions
parseExpr :: Parser Arith
parseExpr = buildExpressionParser table parseInput
  where
    table = [ [ unary  "abs" Abs
              , unary  "-"   Neg
              ]
            , [ binary "^"   (Bin Expo) AssocRight ]
            , [ binary "*"   (Bin Times) AssocLeft 
              , binary "/"   (Bin Divide) AssocLeft
              ]
            , [ binary "+"   (Bin Plus)  AssocLeft
              , binary "-"   (Bin Minus) AssocLeft
              ]
            ]

    unary  name fun       = Prefix (fun <$ reservedOp name)
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc

--The function parses different constants like pi or e.
parseConst :: Parser Const
parseConst = 
      Pi <$ reservedOp "pi"
  <|> E <$ reservedOp "e"

--This function parses the input after we parsed the bounds.
parseInput :: Parser Arith
parseInput = 
  Lit <$> parseFloat <|> X <$ reserved "x" <|> Cons <$> parseConst <|> parens parseExpr

--We parse the bounds of the expression here. Currently does not accept negative values.
parseBound :: Parser Bounds
parseBound = Bound <$> parseFloat <*> (symbol "," *> parseFloat)

--parseFloat converts all double values into float values.
parseFloat :: Parser Float
parseFloat = realToFrac <$> (parseHelpFloat <$> parseNaturalOrDouble)
  
--Here we parse all lit values and renders them as Double values. Rasteriffic does not like
--Double values, it prefers Float values. So, after we return Double, we should convert to Float.
parseHelpFloat :: Either Integer Double -> Double
parseHelpFloat par =
  case par of
    (Left i)    -> fromIntegral i
    (Right dou) -> dou

parseBoundAtom :: Parser Bounds 
parseBoundAtom = whiteSpace *> bracks parseBound

--parses both bounds and artihmetic expressions
--then passes both into constructor BoundedArith
parseBoundedArith :: Parser BoundedAriths
parseBoundedArith = 
  BoundedArith <$> parseBoundAtom <*> (symbol ":" *> parseArith)

parseArith :: Parser Arith
parseArith = whiteSpace *> parseExpr <* eof

--This function inteprets every operator in an arithematic function and applies the operator
interpOp :: Op -> Float -> Arith -> Arith -> Either InterpError Float
interpOp op x a b = 
  case (interpArith x a, interpArith x b) of
    (Right n, Right n1) ->
      case op of
        Divide -> 
          case n1 of
            0 -> Left DivByZero
            _ -> Right(operate op n n1)
        _      -> Right (operate op n n1)
    _                   -> Left InvalidValues

--Here is the main function for interpreting Arithmetic expressions
interpArith :: Float -> Arith -> Either InterpError Float
interpArith x X          = Right x 
interpArith _ (Lit n)      = Right n 
interpArith x (Neg n)      = 
  case interpArith x n of
    (Right a)  -> Right (-a)
    (Left err) -> Left err
interpArith x (Bin op a b) = interpOp op x a b
interpArith _ (Cons co)    = interpConst co
interpArith x (Abs n)      =
  case interpArith x n of
    (Right a)  -> Right (abs a)
    (Left err) -> Left err

--This function interprets the constant and returns the correct value.
interpConst :: Const -> Either InterpError Float
interpConst Pi = Right pi
interpConst E  = Right 2.7 --Approximation of Euler's number. Should work with Float values.

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255

--evalGraph is the main function called, taking in a string and parsing
--for bounds and arithematic expressions.
evalGraph :: String -> Either String Graph
evalGraph input = 
  case parse parseBoundedArith input of
    (Left er)   -> Left (show er)
    (Right (BoundedArith (Bound a b) out)) ->
      drawGraph 256 (samplePoints a b 256) out

--Function responsible for drawing. Calculates points within the bounds
--and then flips them, passing the results to the drawing functions
drawGraph :: Float -> [Float] -> Arith -> Either String Graph
drawGraph x n func = 
  case generatePoints x func of
    (Right points) ->  
      case samePoints points of
        (Right ()) -> Right $
          withTexture (uniformTexture black) $
            stroke 8 JoinRound (CapRound, CapRound) $
              polyline (attachSamplePoints n (flipPoints points))
        (Left ())  ->  Right $
          withTexture (uniformTexture black) $
            stroke 8 JoinRound (CapRound, CapRound) $
              polyline (attachSamplePoints n points)
    (Left err)     -> Left (showInterpError err)

--This function generates points by recursively calling itself and generatePoint.
generatePoints :: Float -> Arith -> Either InterpError [Point]
generatePoints 0 _    = Right []
generatePoints x func = 
  generatePoint x func >>= \xy ->
  generatePoints (x-1) func >>= \points ->
  Right (xy:points)

--This function passes x-values and function into interpArith.
--It returns either an error or a Point.
generatePoint :: Float -> Arith -> Either InterpError Point
generatePoint x func = 
  case interpArith x func of
    (Right y)  -> Right (V2 x y)
    (Left err) -> Left err

--The function checks to see if the whole graph would have the same y-value.
--It is here to check for constants. Before, flipPoints would put constants in the wrong place.
samePoints :: [Point] -> Either () ()
samePoints []         = Right ()
samePoints [(V2 _ _)] = Right ()
samePoints ((V2 _ y):(V2 x y1):rest)
  | y == y1   = samePoints ((V2 x y1):rest)
  | otherwise = Left ()

--A flip function that takes a list of points and applies flipPoint
flipPoints :: [Point] -> [Point]
flipPoints = map flipPoint

--A function that flips a point on the y-axis (so y = 255 would output y = 1)
flipPoint :: Point -> Point
flipPoint (V2 x y) = V2 x (-(y-256))

--From low to high, create n evenly spaced x-coordinates with a gap of (hi-lo)/n.
samplePoints :: Float -> Float -> Float -> [Float]
samplePoints low high n = samplePoint ((high-low)/n) low n 

--Here is a recursive loop that keeps going until mi (min) is larger than ma (max). 
samplePoint :: Float -> Float -> Float -> [Float]
samplePoint gap mi ma
  | mi >= ma = []
  | otherwise  = mi:samplePoint gap (mi+gap) ma

--Meant to resize the graph to create graphs that should go from edge to edge. 
--However, the gap is so small in some cases that the graph would hug an edge.
attachSamplePoints :: [Float] -> [Point] -> [Point]
attachSamplePoints [] []                    = []
attachSamplePoints [] _                     = []
attachSamplePoints _ []                     = []
attachSamplePoints (x:xs) ((V2 _ y):points) = V2 x y:attachSamplePoints xs points