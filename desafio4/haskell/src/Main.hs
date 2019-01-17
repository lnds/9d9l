module Main where

import Data.List
import Data.List.Split
import Data.Ord
import Data.Tuple
import Data.Maybe
import Data.Bits
import Data.Char
import System.Environment
import Control.Monad
import Control.Arrow
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Set as S

data HTree a = Leaf a | Branch (HTree a) (HTree a) deriving (Show, Eq, Ord)

usage = do putStrLn "Uso: huffman [-c|-d] archivo_entrada archivo_salida"

calcFreq = ((length &&& head) <$>) . group . sort

htree ts
  | S.null ts_1 = t1
  | otherwise = htree ts_3
  where
    ((w1, t1), ts_1) = S.deleteFindMin ts
    ((w2, t2), ts_2) = S.deleteFindMin ts_1
    ts_3 = S.insert (w1 + w2, Branch t1 t2) ts_2

buildTree = htree . S.fromList . map (second Leaf)

serializeTree (Branch l r) =
  (second (0 :) <$> serializeTree l) ++ (second (1 :) <$> serializeTree r)
serializeTree (Leaf x) = [(x, [])]

packBits 0 = [0]
packBits 1 = [1]
packBits chr =  packBits (shiftR  chr 1) ++ [chr .&. 0x01]

unpackBits bits = foldl shl 0 bits
  where shl acum bit = (shiftL acum 1) .|. bit

pad8 s | mod (length s) 8 == 0 = s
       | otherwise =  pad8 (0 : s)

padRight8 s | mod (length s) 8 == 0 = s
       | otherwise =  pad8 (s ++ [0])

toBits c  = pad8 (packBits (ord c))

fromBits bits = if length bits == 8 then chr $ unpackBits bits else chr $ unpackBits $ pad8 bits

binarizeTree (Leaf x) = 1 : toBits x
binarizeTree (Branch l r) = 0 : (binarizeTree l) ++ (binarizeTree r)

compress input output = do
  src <- LB.unpack <$> LB.readFile input
  let tree = buildTree $ calcFreq src
  let table = M.fromList $ serializeTree tree
  let lookup chr = fromJust $ M.lookup chr table
  let binaryOut = concat $ map lookup src
  let treeOut = binarizeTree tree
  let outputStream = padRight8 (treeOut ++ binaryOut)
  LB.writeFile output $ LB.pack $ map fromBits (chunksOf 8 outputStream)


readSym bits = (fromBits $ take 8 bits, drop 8 bits)

readLeaf bits = let (sym, rest) = readSym bits
                in (Leaf sym, rest)

readNode bits = let (left, rest1) = readTree bits
                    (right, rest2) = readTree rest1
                    in ((Branch left right), rest2)

readTree [] = error "vacio"
readTree (1:t) = readLeaf t
readTree (0:t) = readNode t
readTree (x:t) = error $ "mal construido: " ++ (show x) ++ ", t: " ++ (show t)

decodeLeaf (Leaf a) bits = (bits, a)
decodeNode (Branch l r) (h:t) = if h == 1 then decodeTree r t else decodeTree l t

decodeTree (Leaf a) bits = decodeLeaf (Leaf a) bits
decodeTree tree bits = decodeNode tree bits

decodeBits tree [] result = result
decodeBits tree bits result =
  let (rest, sym) = decodeTree tree bits
  in decodeBits tree rest (sym : result)

decompress input output = do
  src <- LB.unpack <$> LB.readFile input
  let (tree, bits) = readTree $ (concat $ map toBits src)
  let outputStream = decodeBits tree bits []
  LB.writeFile output $ LB.pack $ reverse outputStream

process ["-c", input, output] = compress input output
process ["-d", input, output] = decompress input output
process _ = usage

main = do
  args <- getArgs
  process args