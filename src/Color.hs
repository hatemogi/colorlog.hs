module Color
  ( mainLoop
  )
where

import           System.IO
import           Data.Char                     as C
import           Data.Maybe                    as M
import           Data.Word
import           Data.Map                      as Map
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Text.Regex.PCRE            as RE (makeRegex, matchTest)
import           Text.Regex.PCRE.ByteString as RE (Regex, compile)

data Color
  = NORMAL | INTEGER | FRACTION | RATIONAL | DATE
  | TIME | IP | DOMAIN | HEXA | UUID | DOUBLEQ
  | SINGLEQ | BRACE | BRACKET | PAREN | DEBUG
  | INFO | WARN | ERROR
      deriving (Eq, Ord, Enum, Show)

colorTable :: Map.Map Color Word8
colorTable = Map.fromList
  [ (NORMAL  , 145)
  , (INTEGER , 142)
  , (FRACTION, 37)
  , (RATIONAL, 104)
  , (DATE    , 38)
  , (TIME    , 169)
  , (IP      , 166)
  , (DOMAIN  , 203)
  , (HEXA    , 178)
  , (UUID    , 169)
  , (DOUBLEQ , 37)
  , (SINGLEQ , 166)
  , (BRACE   , 38)
  , (BRACKET , 142)
  , (PAREN   , 37)
  , (DEBUG   , 145)
  , (INFO    , 146)
  , (WARN    , 178)
  , (ERROR   , 203)
  ]

ansiReset :: B.ByteString
ansiReset = B.pack [0x1b, 91, 48, 109]

prepackedAnsi :: Map.Map Color B.ByteString
prepackedAnsi = Map.fromList
  (zip colors presets)
 where
  ansiBegin = B.pack [0x1b, 91, 51, 56, 58, 53, 58]
  colors = Map.keys colorTable
  presets = Prelude.map (\c -> B.concat [ansiBegin, BC.pack (show (colorTable ! c)), BC.pack "m"]) colors


colorWord :: Color -> B.ByteString -> B.ByteString
colorWord color word = B.concat
  [prepackedAnsi ! color, word, ansiReset]

autoColorWord :: B.ByteString -> B.ByteString
autoColorWord word
  | B.null word
  = word
  | otherwise
  = let (h, t) = (B.head word, B.tail word)
    in  if (h >= 48 && h <= 57) || (h >= 97 && h <= 102) || (h >= 65 && h <= 70)
          then colorWord (autoColorRegex word) word
          else if h == 73 || h == 87 then autoColorInfoOrWarn word else word

prepareRegex :: String -> Regex
prepareRegex = makeRegex

reInteger :: Regex
reInteger = prepareRegex "^[0-9]+$"

reIP :: Regex
reIP = prepareRegex "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]"

reFraction :: Regex
reFraction = prepareRegex "^[0-9]+\\.[0-9]+$"

reRational :: Regex
reRational = prepareRegex "^[0-9]+/[0-9]+$"

reDate :: Regex
reDate = prepareRegex "^[0-9]{4}-[0-9]{2}-[0-9]{2}"

reTime :: Regex
reTime = prepareRegex "^[0-9]+:[0-9]{2}(:[0-9]{2})?"

reUnit :: Regex
reUnit = prepareRegex "^[0-9]+(\\.[0-9]+)?(µs|ms|%|T|G|M|K|B)$"

reHexa :: Regex
reHexa = prepareRegex "^([0-9a-f]{4,})$"

reUUID :: Regex
reUUID = prepareRegex "^([0-9a-f]{8}-[0-9a-f]{4})"

wordDEBUG :: B.ByteString
wordDEBUG = BC.pack "DEBUG"

wordERROR :: B.ByteString
wordERROR = BC.pack "ERROR"

wordWARN :: B.ByteString
wordWARN = BC.pack "WARN"

wordINFO :: B.ByteString
wordINFO = BC.pack "INFO"

autoColorRegex :: B.ByteString -> Color
autoColorRegex word | matchTest reInteger word  = INTEGER
                    | matchTest reIP word       = IP
                    | matchTest reFraction word = FRACTION
                    | matchTest reRational word = RATIONAL
                    | matchTest reDate word     = DATE
                    | matchTest reTime word     = TIME
                    | matchTest reUnit word     = TIME
                    | matchTest reHexa word     = HEXA
                    | matchTest reUUID word     = UUID
                    | word == wordDEBUG         = DEBUG
                    | word == wordERROR         = ERROR
                    | otherwise                 = NORMAL

autoColorInfoOrWarn :: B.ByteString -> B.ByteString
autoColorInfoOrWarn word | word == wordINFO = colorWord INFO word
                         | word == wordWARN = colorWord WARN word
                         | otherwise        = word

closingMap :: Map.Map Word8 Word8
closingMap =
--  Map.fromList [('[', ']'), ('{', '}'), ('(', ')'), ('\'', '\''), ('"', '"')]
  Map.fromList [(91, 93), (123, 125), (40, 41), (39, 39), (34, 34)]

openToColor :: Word8 -> Color
openToColor open = case open of
  91  -> BRACKET
  123 -> BRACE
  40  -> PAREN
  39  -> SINGLEQ
  34  -> DOUBLEQ
  _   -> NORMAL

processOpenLine :: Word8 -> Int -> B.ByteString -> B.ByteString -> [B.ByteString]
processOpenLine open level word line
  | B.null line
  = [colorWord (openToColor open) word]
  | otherwise
  = let (x, xs) = (B.head line, B.tail line)
        closing = closingMap ! open
        newword = word `B.snoc` x
    in  if x == closing && level == 0
          then colorWord (openToColor open) newword : colorLine xs
          else processOpenLine
            open
            ( level
            + (if x == open then 1 else 0)
            - (if x == closing then 1 else 0)
            )
            newword
            xs


wordBegin :: B.ByteString
wordBegin = BC.pack "[{(\'\""

wordSplit :: B.ByteString
wordSplit = BC.pack " ,\t=;\n"

processLine :: B.ByteString -> B.ByteString -> [B.ByteString]
processLine word line = if B.null line
  then [autoColorWord word]
  else
    let (h, t) = (B.head line, B.tail line)
    in  if B.elem h wordBegin
          then autoColorWord word : processOpenLine h 0 (B.singleton h) t
          else if B.elem h wordSplit
            then autoColorWord word : B.singleton h : colorLine t
            else processLine (word `B.snoc` h) t

colorLine :: B.ByteString -> [B.ByteString]
colorLine = processLine B.empty

mainLoop :: IO ()
mainLoop = do
  eof <- isEOF
  if eof
    then return ()
    else do
      line <- B.getLine
      BC.putStrLn (B.concat (colorLine line))
      mainLoop
