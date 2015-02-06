#!/usr/bin/env runhaskell

import Data.List.Split (splitOn)
import Debug.Trace (trace)

main = do
  ls <- getContents >>= return . map (splitOn ",") . lines
  let nodes = getNodes ls
  printSeq nodes ls

getNodes :: [[String]] -> [String]
getNodes ls =
  reverse $ foldl sub [] ls
  where sub acc (a:_:b:_) =
          if elem a acc
          then if elem b acc
               then acc
               else b:acc
          else if elem b $ a:acc
               then a:acc
               else b:a:acc

printSeq :: [String] -> [[String]] -> IO ()
printSeq nodes ls = do
  let maxNodeLen = 2 + (maximum $ map len nodes)
  putStrLn $ concat $ map (centering maxNodeLen) nodes
  mapM_ (printLine nodes maxNodeLen) ls

printLine :: [String] -> Int -> [String] -> IO ()
printLine nodes maxNodeLen (a:c:b:_) = do
  let aIndex = indexOf a nodes
      bIndex = indexOf b nodes
      smaller = min aIndex bIndex
      bigger  = max aIndex bIndex
      printDirection n = putStr $
        case n < smaller || n > bigger of
          True -> '|' : replicate (maxNodeLen - 1) ' '
          False
              | n == aIndex && n+1 == bIndex -> '|' : replicate (maxNodeLen - 2) '-' ++ ">"
              | n == bIndex && n+1 == aIndex -> "|<" ++ replicate (maxNodeLen - 2) '-'
              | n == aIndex && aIndex<bIndex -> '|' : replicate (maxNodeLen - 1) '-'
              | n == aIndex && aIndex>bIndex -> '|' : replicate (maxNodeLen - 1) ' '
              | n == bIndex && aIndex<bIndex -> '|' : replicate (maxNodeLen - 1) ' '
              | n == bIndex && aIndex>bIndex -> "|<" ++ replicate (maxNodeLen - 2) '-'
              | n+1 == aIndex && aIndex<bIndex -> '|' : replicate (maxNodeLen - 1) ' '
              | n+1 == aIndex && aIndex>bIndex -> replicate (maxNodeLen) '-'
              | n+1 == bIndex && aIndex<bIndex -> replicate (maxNodeLen - 1) '-' ++ ">"
              | n+1 == bIndex && aIndex>bIndex -> replicate (maxNodeLen) '-'
              | True                           -> replicate (maxNodeLen) '-'
      printDescription c = do
        let clen = len c
            pos  = max 1 $ ceiling $ fromIntegral (aIndex + bIndex) / 2 * fromIntegral maxNodeLen - fromIntegral clen / 2
            sub a b
                | (b < pos || b >= pos + clen) && rem b maxNodeLen == 0 = '|':a
                | (b < pos || b >= pos + clen)                          = ' ':a
                | b == pos                                              = reverse c ++ a
                | True                                                  = a
        case clen > (maxNodeLen * (length nodes - 1) - 4) of
           True  -> mapM_ printDescription $ chunksByWidth (maxNodeLen * (length nodes - 1) - 4) c
           False -> putStrLn $ reverse $ foldl sub
                    (replicate (div maxNodeLen 2) ' ')
                    [0..(maxNodeLen * (length nodes - 1))]
      printPadding n = putStr $ '|' : replicate (maxNodeLen - 1) ' '
  printDescription c
  --
  putStr $ replicate (div maxNodeLen 2) ' '
  mapM_ printDirection $ take (length nodes) [0..]
  putStrLn ""
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

dprint :: Show a => a -> a
dprint a =
    trace (show a) a