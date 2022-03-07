-- import qualified Text.Parsec.Token as T

module Unison.Tools.Model.ParseSecurityPolicies
  (Unison.Tools.Model.ParseSecurityPolicies.parse,
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


parsePolicy :: Parser (Policy String)
parsePolicy =
  do typ <- parseSecret <|> parseRandom <|> parsePublic
     return typ


parseSecret :: Parser (Policy String)
parseSecret =
  do _ <- string "Secret"; spaces;
     id <- word
     return (Secret id) 

parseRandom :: Parser (Policy String)
parseRandom =
  do _ <- string "Random"; spaces;
     id <- word
     return (Random id)


parsePublic :: Parser (Policy String)
parsePublic =
  do _ <- string "Public"; spaces;
     id <- word
     return (Public id)
