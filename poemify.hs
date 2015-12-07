-- read/write to file
import System.IO
import System.Environment
import Data.Char       
import Control.Arrow

-- partitioning
import Data.List.Split as L
import System.Random

-- sample poems (WCW)
say :: String
say = "I have eaten the plums that were in the icebox and which you were probably saving for breakfast Forgive me they were delicious so sweet and so cold"

red :: String
red = "so much depends upon a red wheel barrow glazed with rain water beside the white chickens."

nl :: String
nl = "\n"

linebreak :: [Int] -> [String] -> [String]
linebreak partition words =
           -- indentation problems
           let f (textLeft, textMade) n =
                 let line = take n textLeft in
                 -- hack for stanzas
                 let stanza =
                       if n `mod` 5 == 0 || n == 1 then [nl] else [] in
                 (drop n textLeft, textMade ++ line ++ [nl] ++ stanza) in
           snd $ foldl f (words, []) partition

-- TODO: better distribution than uniform in the range
randInRange :: Int -> Int -> IO Int
randInRange a b = getStdRandom $ randomR (a, b)

randLen :: IO Int
randLen = randInRange 1 7

-- lazily do it instead of uniformly sampling from the space of all n-partitions
-- probably not mathematically sound, probably not ok to cons len instead of append
randPartition' :: Int -> [Int] -> IO [Int]
randPartition' total lens = do
              len <- randLen
              let totalLeft = total - len
              if (totalLeft < 0)
              then return (len : lens)
              else randPartition' totalLeft (len : lens)

randPartition n = randPartition' n []

-- delete trailing newlines
rstrip = reverse . dropWhile isSpace . reverse

-- for GHCI use
poemify' :: String -> IO ()
poemify' text = do
       let words = L.split (keepDelimsR $ oneOf " ") text
       partition <- randPartition (length words)
       let poem = linebreak partition words
       putStrLn ((rstrip $ concat poem) ++ nl)

-- TODO: stanzas, not just line breaks
poemify :: String -> IO String
poemify text = do
       let words = L.split (keepDelimsR $ oneOf " ") text
       partition <- randPartition (length words)
       let poem = linebreak partition words
       return ((rstrip $ concat poem) ++ nl)

-- ghc poemify.hs
-- ./poemify <filename>
main = do
     args <- getArgs
     let inName = head args
     prose <- readFile $ inName
     poem <- poemify prose
     writeFile ("poem_" ++ inName) poem
