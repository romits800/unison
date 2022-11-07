-- import qualified Text.Parsec.Token as T

module Unison.ParseSecurityPolicies
  (Unison.ParseSecurityPolicies.parse,
   Policy (..)) where
-- import Text.Parsec.Language (emptyDef)
import Text.Parsec
-- import Text.Parsec.Language (haskellStyle)
import Text.ParserCombinators.Parsec as P

data Policy a =
  Secret a |
  Public a |
  Random a
  deriving (Show, Eq, Ord)


parse :: String -> [Policy String]
parse input = 
  case (P.parse policyparser [] input) of
    Left err -> error ("parse error: " ++ show err)
    Right x  -> x


policyparser :: Parser [Policy String]
policyparser =
  do types <- sepBy parsePolicy sep;
     eof <|> (skipMany1 newline);
     return types


sep :: Parser ()
sep = do {char ';'; skipMany1 (space <|> newline)}


word :: Parser String
word = many1 (alphaNum <|> char '_')

memr :: Parser Char
memr = char ']'

meml :: Parser Char
meml = char '['



parsePolicy :: Parser (Policy String)
parsePolicy =
  do typ <- parseSecret <|> parseRandom <|> parsePublic <|>
            parseSecretMem <|> parseRandomMem <|> parsePublicMem
     return typ


parseSecret :: Parser (Policy String)
parseSecret =
  do _ <- string "Secret"; spaces;
     id <- word
     return (Secret id) 

parseSecretMem :: Parser (Policy String)
parseSecretMem =
  do _ <- string "secMem"; spaces; meml;
     id <- word; memr
     return (Secret ("[" ++ id ++ "]")) 


parseRandom :: Parser (Policy String)
parseRandom =
  do _ <- string "Random"; spaces;
     id <- word
     return (Random id)

parseRandomMem :: Parser (Policy String)
parseRandomMem =
  do _ <- string "randMem"; spaces; meml
     id <- word; memr
     return (Random ("[" ++ id ++ "]"))



parsePublic :: Parser (Policy String)
parsePublic =
  do _ <- string "Public"; spaces;
     id <- word
     return (Public id)

parsePublicMem :: Parser (Policy String)
parsePublicMem =
  do _ <- string "pubMem"; spaces; meml
     id <- word; memr
     return (Public ("[" ++ id ++ "]"))
