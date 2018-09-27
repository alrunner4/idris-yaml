module YAML
import public Control.Monad.Identity
import public Data.SortedMap
import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

% access public export

data YamlValue = YamlString String
               | YamlNumber Double
               | YamlBool Bool
               | YamlNull
               | YamlObject( SortedMap String YamlValue )
               | YamlArray( List YamlValue ) -- TODO: YamlObject

intercalate : String -> List String -> String
intercalate sep Nil = ""
intercalate sep [x] = x
intercalate sep ( x :: xs ) = x ++ sep ++ intercalate sep xs

implementation Show YamlValue where
	show( YamlString s     ) = show s
	show( YamlNumber x     ) = show x
	show( YamlBool   True  ) = "true"
	show( YamlBool   False ) = "false"
	show  YamlNull           = "null"
	show( YamlObject xs    ) = "{" ++ YAML.intercalate ", " ( (\ k,v => k ++ ": " ++ show v ) <$> SortedMap.toList xs ) ++ "}"
	show( YamlArray  xs    ) = show xs

hex : Parser Int
hex = do
	c <- ord . toUpper <$> satisfy isHexDigit
	pure $ if c >= ord '0' && c <= ord '9'
		then c - ord '0'
		else 10 + c - ord 'A'

hexQuad : Parser Int
hexQuad = do
	a <- hex
	b <- hex
	c <- hex
	d <- hex
	pure( a * 4096 + b * 256 + c * 16 + d )

specialChar : Parser Char
specialChar = do
	c <- satisfy( const True )
	case c of
		'"'  => pure '"'
		'\\' => pure '\\'
		'/'  => pure '/'
		'b'  => pure '\b'
		'f'  => pure '\f'
		'n'  => pure '\n'
		'r'  => pure '\r'
		't'  => pure '\t'
		'u'  => chr <$> hexQuad
		_    => fail "expected special char"

export
yamlString' : Parser( List Char )
yamlString' = ( char '"' *!> pure Prelude.List.Nil ) <|> do
	c <- satisfy( /= '"' )
	if ( c == '\\' )
		then (::) <$> specialChar <*> yamlString'
		else (::) <$> c           <*> yamlString'

||| parse a string containing YAML-recognized escape sequences
yamlString : Parser String
yamlString = char '"' *> map pack yamlString' <?> "Yaml string"

-- inspired by Haskell's Data.Scientific module
data Scientific = MkScientific Integer Integer

scientificNum : Num a => Fractional a => Scientific -> a
scientificNum( MkScientific b e ) = fromInteger b * if e < 0
  then 1 / pow 10 ( fromIntegerNat( -e ) )
  else pow 10 ( fromIntegerNat e )

parseScientific : Parser Scientific
parseScientific = do
  sign        <- maybe 1 ( const (-1) ) <$> opt ( char '-' )
  digits      <- some digit
  hasComma    <- isJust <$> opt( char '.' )
  decimals    <- if hasComma then some digit else pure Prelude.List.Nil
  hasExponent <- isJust <$> opt ( char 'e' )
  exponent    <- if hasExponent then integer else pure 0
  let fromDigits = foldl(\ a,b => 10 * a + cast b ) 0
  pure $ MkScientific( sign * fromDigits( digits ++ decimals ) )( exponent - cast( length decimals ) )

yamlNumber : Parser Double
yamlNumber = scientificNum <$> parseScientific

yamlBool : Parser Bool
yamlBool =  ( char 't' >! string "rue"  *> pure True  )
        <|> ( char 'f' >! string "alse" *> pure False )
        <?> "Yaml Bool"

yamlNull : Parser ()
yamlNull = ( char 'n' >! string "ull" >! pure () ) <?> "Yaml Null"

||| a parser to skip same-line whitespace
yamlSpace : Monad m => ParserT String m ()
yamlSpace = skip( many( satisfy(\ c => c /= '\n' && isSpace c ) ) ) <?> "yamlSpace"

mutual

    yamlArray : Parser( List YamlValue )
    yamlArray = char '-' *!> yamlArrayValue `sepBy` ( char '-' )

    keyValuePair : Parser( String , YamlValue )
    keyValuePair = do key <- map pack (many (satisfy $ not . isSpace)) <* spaces -- This `space` is improper.
                      val <- char ':' *> yamlValue
                      pure (key, val)

    yamlObject : Parser( SortedMap String YamlValue )
    yamlObject = map fromList $ keyValuePair `sepBy` (char '\n') --(pure '\n')

    -- TODO check id indent is bigger than in array start
    yamlObjectA : Parser (SortedMap String YamlValue)
    yamlObjectA = map fromList $ keyValuePair `sepBy` (char '\n') --(pure '\n')

    yamlValue' : Parser YamlValue
    yamlValue' =    YamlString <$> dquote 
            <|>     YamlNumber <$> yamlNumber
            <|>       YamlBool <$> yamlBool
            <|>( pure YamlNull <*  yamlNull )
            <|>|     YamlArray <$> yamlArray
            <|>|    YamlObject <$> yamlObject

    yamlArrayValue : Parser YamlValue
    yamlArrayValue = spaces *> yamlValue' <* spaces

    yamlValue : Parser YamlValue
    yamlValue = yamlSpace *> yamlValue' <* yamlSpace


yamlToplevelValue : Parser YamlValue
yamlToplevelValue = map YamlArray yamlArray <|> map YamlObject yamlObject
