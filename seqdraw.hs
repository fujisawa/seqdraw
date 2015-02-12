#!/usr/bin/env runhaskell

import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Text.Parsec
import System.IO (hPutStrLn, stderr)

main = do
  cs <- getContents
  case parse parseSeqs "parseSeq" cs of
    Left e  -> hPutStrLn stderr $ "parse error : " ++ show e
    Right e -> do let nodes = getNodes e
                  printSeq nodes e

data Seq = SeqNodes [String] | SeqLine [String] | SeqIf String | SeqEndif deriving Show

parseSeqs =
  sepBy parseSeq newline

parseSeq = parseNodes <|> parseEndif <|> parseElif <|> parseIf <|> parseLine

parseNodes = do
  try $ string "#nodes:"
  sepBy1 parseNode (char ',') >>= return . SeqNodes

parseEndif = do
  try $ string "#endif"
  return SeqEndif

parseIf = do
  try $ string "#if:"
  (many1 $ noneOf "\n") >>= return . SeqIf

parseElif = do
  try $ string "#elif:"
  (many1 $ noneOf "\n") >>= return . SeqIf

parseNode = do
  spaces
  x <- many1 $ noneOf "\n,"
  return $ trim x

parseLine = do
  x <- sepBy1 parseColumn (char ',')
  return $ SeqLine x

parseColumn = do
  spaces
  x <- many $ noneOf ",\n"
  return $ trim x

getNodes :: [Seq] -> [String]
getNodes ls =
  reverse $ foldl sub [] ls
  where sub acc (SeqNodes es) =
          reverse es ++ acc
        sub acc (SeqLine (a:_:b:_)) =
          if elem a acc
          then if elem b acc
               then acc
               else b:acc
          else if elem b $ a:acc
               then a:acc
               else b:a:acc
        sub acc _ =
          acc

printSeq :: [String] -> [Seq] -> IO ()
printSeq nodes ls = do
  let blockWidth = max (quot 40 $ length nodes) $ 2 + (maximum $ map width nodes)
  putStrLn $ concat $ map (centering blockWidth) nodes
  mapM_ (printLine nodes blockWidth) ls

printLine :: [String] -> Int -> Seq -> IO ()
printLine _ _ (SeqNodes _) = return ()
printLine nodes blockWidth (SeqEndif) = do
      let blocksWidth = blockWidth * length nodes
      putStrLn $ replicate blocksWidth '='
printLine nodes blockWidth (SeqIf state) = do
      let blocksWidth = blockWidth * length nodes
      putStrLn $ state ++ ' ' : replicate (blocksWidth - width state - 1) '='
printLine nodes blockWidth (SeqLine (a:c:b:_)) = do
  let aIndex = indexOf a nodes
      bIndex = indexOf b nodes
      smaller = min aIndex bIndex
      bigger  = max aIndex bIndex
      blocksWidth = blockWidth * length nodes
      printDirection = do
        let sub a n
                | aIndex < bIndex && n == bIndex * blockWidth - 1          = '>':a
                | aIndex > bIndex && n == bIndex * blockWidth + 1          = '<':a
                | (n > smaller * blockWidth) && (n < bigger * blockWidth)  = '-':a
                | rem n blockWidth == 0                                    = '|':a
                | True                                                     = ' ':a
        putStrLn $ reverse $ foldl sub
          ""
          $ map (flip (-) (truncate $ fromIntegral blockWidth/2)) [0..(blocksWidth - 1)]
      printDescription c = do
        let cwidth = width c
            pos  = min (blocksWidth - cwidth) $ max 1 $ ceiling $ fromIntegral (aIndex + bIndex) / 2 * fromIntegral blockWidth - fromIntegral cwidth / 2 + fromIntegral blockWidth / 2
            sub a b
                | (b < pos || b >= pos + cwidth) && rem b blockWidth == quot blockWidth 2 = '|':a
                | (b < pos || b >= pos + cwidth)                          = ' ':a
                | b == pos                                              = reverse c ++ a
                | True                                                  = a
        case cwidth > (blocksWidth - 4) of
           True  -> mapM_ printDescription $ chunksByWidth (chunkSize cwidth (blocksWidth - 4)) c
           False -> putStrLn $ reverse $ foldl sub
                    ""
                    [0..(blocksWidth - 1)]
      printPadding = do
        let sub a n
                | rem n blockWidth == 0                                    = '|':a
                | True                                                     = ' ':a
        putStrLn $ reverse $ foldl sub
          ""
          $ map (flip (-) (truncate $ fromIntegral blockWidth/2)) [0..(blocksWidth - 1)]
  printPadding
  --
  printDescription c
  --
  printDirection

chunkSize :: Int -> Int -> Int
chunkSize a b =
    fdiv a $ fdiv a b

fdiv :: Int -> Int -> Int
fdiv a b =
    case divMod a b of
      (d, 0) -> d
      (d, _) -> d + 1

indexOf :: Eq a => a -> [a] -> Int
indexOf a al =
    let (m1, m2) = break (a ==) al
    in length m1

padding pwidth w =
  w ++ replicate (pwidth - width w) ' '

righting pwidth w =
  replicate (pwidth - width w) ' ' ++ w

centering pwidth w =
    let fwidth = div (pwidth - width w) 2
        bwidth = pwidth - width w - fwidth
    in replicate fwidth ' ' ++ w ++ replicate bwidth ' '

width =
  sum . map charWidth
  where charWidth c =
          if elem c [' '..'~']
          then 1
          else 2

chunksByWidth :: Int -> String -> [String]
chunksByWidth n str =
    sub 0 [] [] str
    where sub _ [] cacc [] =
              reverse cacc
          sub _ wacc cacc [] =
              reverse $ reverse wacc:cacc
          sub wwidth wacc cacc (c:str) =
              let wwidth' = width [c] + wwidth
              in if wwidth' > n
                 then sub 0 "" (reverse wacc:cacc) $ c:str
                 else sub wwidth' (c:wacc) cacc str

trim = reverse . ltrim . reverse . ltrim

ltrim = snd . break (flip notElem " \t")

dprint :: Show a => a -> a
dprint a =
    trace (show a) a