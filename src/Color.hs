module Color
  ( mainLoop
  )
where

import System.IO
import Data.Char             as C
import Data.Maybe            as M
import Data.Word
import Data.Map              as Map
import Data.ByteString       as B
import Data.ByteString.Char8 as BC

data Color
  = NORMAL | INTEGER | FRACTION | RATIONAL | DATE
  | TIME | IP | DOMAIN | HEXA | UUID | DOUBLEQ
  | SINGLEQ | BRACE | BRACKET | PAREN | DEBUG
  | INFO | WARN | ERROR
      deriving (Enum, Show)

colorTable :: Color -> Word8
colorTable color =
  [ 145
    , 142
    , 37
    , 104
    , 38
    , 169
    , 166
    , 203
    , 178
    , 169
    , 37
    , 166
    , 38
    , 142
    , 37
    , 145
    , 146
    , 178
    , 203
    ]
    !! fromEnum color

ansiBegin :: ByteString
ansiBegin = B.pack [0x1b, 91, 51, 56, 58, 53, 58]

ansiReset :: ByteString
ansiReset = B.pack [0x1b, 91, 48, 109]

colorWord :: Color -> ByteString -> ByteString
colorWord color word =
  ansiBegin
    `append` BC.pack (show (colorTable color))
    `append` BC.pack "m"
    `append` word
    `append` ansiReset

closingMap :: Map Word8 Word8
closingMap =
--  Map.fromList [('[', ']'), ('{', '}'), ('(', ')'), ('\'', '\''), ('"', '"')]
  Map.fromList [(91, 93), (123, 125), (40, 41), (39, 39), (34, 34)]

closingFor :: Word8 -> Word8
closingFor char = closingMap ! char


openToColor :: Word8 -> Color
openToColor open = case open of
  91  -> BRACKET
  123 -> BRACE
  40  -> PAREN
  39  -> SINGLEQ
  34  -> DOUBLEQ
  _   -> NORMAL

processOpenLine :: Word8 -> Int -> ByteString -> ByteString -> ByteString
processOpenLine open level word line = if B.null line
  then colorWord (openToColor open) word
  else
    let (x, xs) = (B.head line, B.tail line)
        closing = closingFor open
        newword = word `B.snoc` x
        newlevel =
          (case (x == open, x == closing) of
            (True , False) -> level + 1
            (False, True ) -> level - 1
            _              -> level
          )
    in  if x == closing && level == 0
          then colorWord (openToColor open) newword `append` colorLine xs
          else processOpenLine open newlevel newword xs


processLine :: B.ByteString -> B.ByteString -> B.ByteString
processLine word line = if B.null line
  then word
  else
    let h = B.head line
        t = B.tail line
    in  if B.elem h (BC.pack "[{(\'\"")
          then word `append` processOpenLine h 0 (B.singleton h) t
          else processLine (word `B.snoc` h) t

colorLine :: B.ByteString -> B.ByteString
colorLine = processLine B.empty

mainLoop :: IO ()
mainLoop = do
  line <- B.getLine
  eof <- isEOF
  if eof
    then System.IO.putStr ""
    else do
      BC.putStrLn (colorLine line)
      mainLoop
