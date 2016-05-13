module FOMObot.Types.Command where

import Data.Either (rights)
import Text.Parsec (parse, manyTill)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, anyChar, char)

data Command
    = Add [String]
    | Remove [String]
    | List
    | Stop
    | Help
    | Unknown
    deriving Show

parseCommand :: String -> Command
parseCommand s =
    case words s of
      "add":xs -> Add $ parseChannels xs
      "remove":xs -> Remove $ parseChannels xs
      "list":_ -> List
      "stop":_ -> Stop
      "help":_ -> Help
      _ -> Unknown
  where
    parseChannels xs = rights $ (parse parser "") <$> xs

    parser :: Parser String
    parser = (string "<#") *> (manyTill anyChar $ char '>')
