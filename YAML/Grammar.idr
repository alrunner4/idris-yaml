module YAML.Grammar
import Lightyear
import Lightyear.Char
import Lightyear.Strings

% language TypeProviders

anychar : Parser Char
anychar = satisfy(\ _ => True )

anystring : Parser String
anystring = pack <$> many anychar

word : Parser String
word = spaces *> ( pack <$> many( satisfy( not . isSpace ) ) <?> "word" ) <* spaces

||| This rendition of block comments ignores all whitespace:
|||   words are joined by single spaces regardless of source presentation.
comment : Parser String
comment = unwords <$> ( token "/*" *> manyTill word ( token "*/" ) )

namespace Test
  comment : IO ()
  comment = case parse comment "/* this is the song that doesn't end\n  because it's multiline!  */" of
    Left  err => putStrLn err
    Right val => printLn val
