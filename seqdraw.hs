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

data Seq = SeqNodes [String] | SeqLine [String] deriving Show

parseSeqs =
  sepBy parseSeq newline

parseSeq = parseNodes <|> parseLine

parseNodes = do
  string "#nodes:"
  sepBy1 parseNode (char ',') >>= return . SeqNodes

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

printSeq :: [String] -> [Seq] -> IO ()
printSeq nodes ls = do
  let maxNodeLen = 2 + (maximum $ map len nodes)
  putStrLn $ concat $ map (centering maxNodeLen) nodes
  mapM_ (printLine nodes maxNodeLen) ls

printLine :: [String] -> Int -> Seq -> IO ()
printLine _ _ (SeqNodes _) = return ()
printLine nodes maxNodeLen (SeqLine (a:c:b:_)) = do
  let aIndex = indexOf a nodes
      bIndex = indexOf b nodes
      smaller = min aIndex bIndex
      bigger  = max aIndex bIndex
      printDirection = do
        let sub a n
                | aIndex < bIndex && n == bIndex * maxNodeLen - 1          = '>':a
                | aIndex > bIndex && n == bIndex * maxNodeLen + 1          = '<':a
                | (n > smaller * maxNodeLen) && (n < bigger * maxNodeLen)  = '-':a
                | rem n maxNodeLen == 0                                    = '|':a
                | True                                                     = ' ':a
        putStrLn $ reverse $ foldl sub
          (replicate (div maxNodeLen 2) ' ')
          [0..(maxNodeLen * (length nodes - 1))]
      printDescription c = do
        let clen = len c
            pos  = min (maxNodeLen * length nodes - clen) $ max 1 $ ceiling $ fromIntegral (aIndex + bIndex) / 2 * fromIntegral maxNodeLen - fromIntegral clen / 2 + fromIntegral maxNodeLen / 2
            sub a b
                | (b < pos || b >= pos + clen) && rem b maxNodeLen == quot maxNodeLen 2 = '|':a
                | (b < pos || b >= pos + clen)                          = ' ':a
                | b == pos                                              = reverse c ++ a
                | True                                                  = a
        case clen > (maxNodeLen * (length nodes - 1) - 4) of
           True  -> mapM_ printDescription $ chunksByWidth (maxNodeLen * (length nodes - 1) - 4) c
           False -> putStrLn $ reverse $ foldl sub
                    ""
                    [0..(maxNodeLen * length nodes)]
      printPadding n = putStr $ '|' : replicate (maxNodeLen - 1) ' '
  printDescription c
  --
  printDirection
  --
  putStr $ replicate (div maxNodeLen 2) ' '
  mapM_ printPadding $ take (length nodes) [0..]
  putStrLn ""

indexOf :: Eq a => a -> [a] -> Int
indexOf a al =
    let (m1, m2) = break (a ==) al
    in length m1

padding plen w =
  w ++ replicate (plen - len w) ' '

righting plen w =
  replicate (plen - len w) ' ' ++ w

centering plen w =
    let flen = div (plen - len w) 2
        blen = plen - len w - flen
    in replicate flen ' ' ++ w ++ replicate blen ' '

len =
  sum . map charLen
  where charLen c =
          if elem c [' '..'~']
          then 1
          else 2

chunksByWidth :: Int -> String -> [String]
chunksByWidth n str =
    sub 0 [] [] str
    where sub _ [] cacc [] =
              reverse cacc
          sub _ wacc cacc [] =
              reverse $ wacc:cacc
          sub wlen wacc cacc (c:str) =
              let wlen' = len [c] + wlen
              in if wlen' > n
                 then sub 0 "" (reverse wacc:cacc) str
                 else sub wlen' (c:wacc) cacc str

trim = reverse . ltrim . reverse . ltrim

ltrim = snd . break (flip notElem " \t")

dprint :: Show a => a -> a
dprint a =
    trace (show a) a