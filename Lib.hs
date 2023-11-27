module Lib where 

import System.Process
import System.IO  
import Control.Monad


data SMT = Arg [String]| Smt String [SMT]

test :: SMT 
test = Smt "assert" [(Smt "forall" [Smt "row" [Arg ["Int"]], Smt "i" [Arg ["Int"]], Smt "j" [Arg ["Int"]]])]

stringlst2string :: [String] -> String 
stringlst2string [] = ""
stringlst2string [h] = h
stringlst2string (h:t) = h ++ " " ++ (stringlst2string t)

smt2string :: SMT -> String
smt2string (Arg lst) = stringlst2string lst 
smt2string (Smt name lst) = "(" ++ name ++ " " ++ (stringlst2string (map smt2string lst)) ++ ")" 

smt :: [String] 
smt = ["(set-option :produce-models true)", 
       "(declare-datatypes () ((Val V1 V2 V3 V4 V5 V6 V7 V8 V9)))",
       "(declare-fun board (Int Int) Val)"]

list2string :: [String] -> String
list2string [] = ""
list2string (h:t) = h ++ "\n" ++ list2string t 

writeSMT2 :: [String] -> IO ()
writeSMT2 content = writeFile "file.txt" (list2string (content ++ [smt2string test]))

callZ3 :: IO String
callZ3 = (readProcess "z3" ["sudoku.smt2"] "") 

f :: [String] -> [[Int]]
f l = map (map read) (map words l)

main = do  
        content <- readFile ("puzzle.txt")
        let linesOfFiles = f (lines content)
        print linesOfFiles 
        writeSMT2 smt
        callZ3
