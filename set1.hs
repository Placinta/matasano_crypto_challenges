{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Bits as B
import Data.Char
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Word
import Data.Maybe
import Data.List (maximumBy)
import Data.Function (on)

set1Challenge1HexString = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
base64Table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

hexToBS s = fst $ B16.decode s
hexToBase64 s = B64.encode $ hexToBS s  

convertAndShowHexToBase64 hexString = do 
	putStrLn "Matasano Set 1 Challenge 1."
	putStr "Initial hex string: "
	BSC.putStrLn hexString
	putStr "Base64 encoded string: "
	BSC.putStrLn $ hexToBase64 hexString
	putStrLn ""
	--putStr "Secret: "
	--let Right secret = B64.decode $ hexString  
	--BSC.putStrLn secret
set1Challenge1 = convertAndShowHexToBase64 set1Challenge1HexString


xorTwoHexByteStrings s1 s2 = xorTwoByteStrings (hexToBS s1) (hexToBS s2)
xorTwoByteStrings s1 s2 = BS.pack $ BS.zipWith B.xor s1 s2

set1Challenge2String1 = "1c0111001f010100061a024b53535009181c"
set1Challenge2String2 = "686974207468652062756c6c277320657965"
set1Challenge2 = do
	putStrLn "Matasano Set 1 Challenge 2."
	putStr "String 1: "
	BSC.putStrLn set1Challenge2String1
	putStr "String 2: "
	BSC.putStrLn set1Challenge2String2
	putStr "Hex encoded xored string: "
	let xoredString = xorTwoHexByteStrings set1Challenge2String1 set1Challenge2String2
	BSC.putStrLn $ B16.encode xoredString
	putStr "ASCII encoded xored string: "
	BSC.putStrLn xoredString
	putStrLn ""



replicateCharN length char = BS.replicate length $ fromIntegral $ ord char

createXorKey originalString repeatedLetter = replicateCharN (BS.length originalString) repeatedLetter
displayXorString s = BSC.putStrLn s 

letterFrequencyForEnglish = 
	[
	('a', 8.167), ('b', 1.492), ('c', 2.782), ('d', 4.253), ('e', 13.0001), 
	('f', 2.228), ('g', 2.015), ('h', 6.094), ('i', 6.966), ('j', 0.153), 
	('k', 0.772), ('l', 4.025), ('m', 2.406), ('n', 6.749), ('o', 7.507), 
	('p', 1.929), ('q', 0.095), ('r', 5.987), ('s', 6.327), ('t', 9.056), 
	('u', 2.758), ('v', 0.978), ('w', 2.360), ('x', 0.150),	('y', 1.974), 
	('z', 0.074)
	]
englishLetterFrequencyMap = M.fromList $ listOfCharsAndFreqsToWordsAndFreqs letterFrequencyForEnglish


listOfCharsAndFreqsToWordsAndFreqs l = map helper l
	where helper (char, freq) = (BS.c2w char, freq)

frequencySum = foldl f 0 (listOfCharsAndFreqsToWordsAndFreqs letterFrequencyForEnglish)
	where f acc (_, c) = acc + c

computeStringLetterFrequency string = BS.foldl freqHelper M.empty string
	where freqHelper freqCounter character = M.insertWith (+) character 1 freqCounter 

-- Return a higher score for common chars other than english letters, and a negative one for less frequently used symbols.
letterScoreAdditionalAdjustments letter maybeLetterScore = 
	case maybeLetterScore of 
		Just letterScore -> letterScore
		Nothing -> case BS.w2c letter of
			' ' -> 1
			'\'' -> 1
			_ -> -1 

-- Computes the score for a given string, how much it looks like an English sentence,
-- by doing simple linear regression of count of letters found in string, multiplied by the letter frequency.
englishScore sentence = M.foldrWithKey letterScore 0 stringLetterFrequency 
	where 
		letterScore char frequency acc = acc + (englishLetterFrequency char) * frequency
		stringLetterFrequency = computeStringLetterFrequency sentence
		englishLetterFrequency l = letterScoreAdditionalAdjustments l (M.lookup l englishLetterFrequencyMap)

getEnglishScoresOfString (AnalyzedString sentence _ key) = AnalyzedString sentence (englishScore sentence) key
findMostProbablyEnglishSentence listOfScoresAndStrings = maximumBy (compare `on` getAnalyzedStringScore) listOfScoresAndStrings

-- The initial hex string xored with a secret key
set1Challenge3String = hexToBS "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
set1Challenge3StringXoredWithKey repeatLetter = AnalyzedString xoredString 0 createdXorKey
	where
		xoredString =  xorTwoByteStrings set1Challenge3String createdXorKey
		createdXorKey = createXorKey set1Challenge3String repeatLetter

data AnalyzedString a = AnalyzedString {
	string :: a,
	score :: Double,
	key :: a
}

getAnalyzedStringScore (AnalyzedString _ s _) = s
getAnalyzedStringSentence (AnalyzedString s _ _) = s
getAnalyzedStringKey (AnalyzedString _ _ s) = s

set1Challenge3 = do
	let charactersToXorWith = ['a'..'z'] ++ ['A'..'Z']
	let xoredStringsAndKeys = map set1Challenge3StringXoredWithKey charactersToXorWith
	let listOfScoresAndStringsAndKeys = map getEnglishScoresOfString xoredStringsAndKeys
	let mostProbableEnglishSentence = findMostProbablyEnglishSentence listOfScoresAndStringsAndKeys
	--mapM_ (\e -> putStrLn $ show e) listOfScoresAndStrings
	putStrLn "Matasano Set 1 Challenge 3."
	putStr "Given encrypted string: "
	BSC.putStrLn set1Challenge3String
	putStr "Most probable decrypted English Sentence: "
	BSC.putStrLn $ getAnalyzedStringSentence mostProbableEnglishSentence
	putStr "Decrypted with key: "
	BSC.putStrLn $ getAnalyzedStringKey mostProbableEnglishSentence

main = do
	set1Challenge1
	set1Challenge2
	set1Challenge3








