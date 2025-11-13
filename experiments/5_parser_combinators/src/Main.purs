module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)

myParser :: Parser { intField1 :: Int, strField2 :: String }
myParser = ado
  intField1 <- getInt
  strField2 <- getStr
  in { intField1, strField2 }

main :: Effect Unit
main = do
  case (parseArgs ["40", "val1"] myParser) of
    Left err -> log $ "Got unexpected error: " <> err
    Right parsed -> log $ "Successfully parsed " <> show parsed.intField1 <> " and " <> parsed.strField2
  
  case (parseArgs ["abc"] myParser) of
    Left err | err == "Couldn't parse int" -> log "Got expected error"
             | otherwise -> log $ "Got unexpected error: " <> err
    Right parsed -> log $ "Failed to get error, parsed " <> show parsed

data Parser a
  = Parser (Array String -> Either String { parsed :: a, rest :: Array String })

parseArgs :: forall a. Array String -> Parser a -> Either String a
parseArgs args (Parser f) = _.parsed <$> f args

instance parserFunctor :: Functor Parser where
  map f (Parser g) = Parser \args -> case (g args) of
    Left err -> Left err
    Right result -> Right result { parsed = f result.parsed }

instance applyParser :: Apply Parser where
  apply (Parser f) (Parser g) = Parser \args -> do
    fResult <- f args
    gResult <- g fResult.rest
    pure { parsed: fResult.parsed gResult.parsed, rest: gResult.rest }

instance applicativeParser :: Applicative Parser where
  pure x = Parser \args -> Right { parsed: x, rest: args }

getStr :: Parser String
getStr = Parser \args -> case (uncons args) of
  Nothing -> Left "No more commands"
  Just { head, tail } -> Right { parsed: head, rest: tail }

getInt :: Parser Int
getInt = Parser \args -> case (uncons args) of
  Nothing -> Left "No more commands"
  Just { head, tail } -> case (fromString head) of
    Nothing -> Left "Couldn't parse int"
    Just i -> Right { parsed: i, rest: tail }
