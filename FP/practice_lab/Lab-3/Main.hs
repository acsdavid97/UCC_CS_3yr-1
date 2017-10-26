{-
 - Name: Acs David.
 - Number: 117106523.
 - Assignment: 02.
 -}

module Main where

import Data.List
import Data.List.Split
import Data.String
import Data.Char

-- vowels in the English language
vowels = "aeiou"

-- checks if a character is a vowel
isVowel :: Char -> Bool
isVowel c = c `elem` vowels

-- checks if a character is a consonant
isConsonant :: Char -> Bool
isConsonant c = isLower c && not (isVowel c)

-- translates a single word to Pig Latin if the word starts with a consonant
-- the letters before an initial vowel are moved to the end of the word 
-- and finally "ay" is added at the end of the word.
consonant_word_translate :: String -> String
consonant_word_translate word@(c:_) | isConsonant c = second ++ first ++ "ay"
                                    | otherwise = word
    where (first, second) = span isConsonant word

-- translates a single word to Pig Latin if the word starts with a vowel
-- the "way" string is appended at the end of the string
vowel_word_translate :: String -> String
vowel_word_translate word@(c:_) | isVowel c = word ++ "way"
                                | otherwise = word

-- combines the consonant and the vowel rules of pig latin
-- note that the order of the function applications is important
translate_word_both :: String -> String
translate_word_both = consonant_word_translate . vowel_word_translate

-- splits a string into word and non-word sub lists using two helper functions
-- for example for "aaaa...." the function will return ["aaaa", "...."]
tokenize :: String -> [String]
tokenize [] = []
tokenize str@(c:_) | isLower c = get_first_lowers str
                   | otherwise = get_first_non_lowers str

-- helper function for the tokenize function.
-- this function will get the first contiguous lowercase characters and
-- call the twin helper function to get the other characters.
get_first_lowers :: String -> [String]
get_first_lowers [] = []
get_first_lowers str = lowers : get_first_non_lowers non_lowers
    where (lowers, non_lowers) = span isLower str

-- helper function for the tokenize function.
-- this function will get the first contiguous non-lowercase characters and
-- call the twin helper function to get the other characters.
get_first_non_lowers :: String -> [String]
get_first_non_lowers [] = []
get_first_non_lowers str = non_lowers : get_first_lowers lowers
    where (non_lowers, lowers) = break isLower str

-- translates a string using the word translate rule specified in the first 
-- parameter. Tokenizes the string, translates each word resulting from the 
-- tokenization and stitches the resulting strings.
translate :: (String -> String) -> String -> String
translate word_translator str = concatMap word_translator (tokenize str)

-- applies the vowel_translate rule for a string
vowel_translate :: String -> String
vowel_translate = translate vowel_word_translate

-- applies the consonant_translate rule for a string
consonant_translate :: String -> String
consonant_translate = translate consonant_word_translate

-- applies the vowel and consoant translate rules for a string
translate_both :: String -> String
translate_both = translate translate_word_both 

main :: IO ()
main = do line <- getLine
          putStrLn (translate_both line)
