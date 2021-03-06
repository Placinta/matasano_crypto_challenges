{-# LANGUAGE OverloadedStrings, BangPatterns, ViewPatterns #-}

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BB
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
import Data.List (maximumBy, minimumBy)
import Data.Function (on)
import Debug.Trace
import System.IO
import qualified Data.ByteString.Lazy.Lens as BS
import qualified Data.Bits.Lens as BL
import Control.Lens
import Data.Int
import Data.Monoid
import qualified Data.List as L
import qualified Codec.Crypto.AES as AES

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



replicateCharN length char = BS.replicate length $ fromIntegral char

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

-- Returns a map of character => count values.
computeStringLetterFrequency string = BS.foldl freqHelper M.empty string
	where freqHelper freqCounter character = M.insertWith (+) character 1 freqCounter 

letterScoreAdditionalAdjustments letter maybeLetterScore = 
	case maybeLetterScore of 
		Just letterScore -> letterScore
		Nothing -> case BS.w2c letter of
			' ' -> -10
			'\'' -> 0
			_ -> 0

-- Returns a score of 20, for all non english chars.
nonEnglishLetterScore letter maybeLetterScore =
	case maybeLetterScore of 
		Just letterScore -> 0
		Nothing -> 20


-- Computes the score for a given string, how much it looks like an English sentence,
-- by doing simple linear regression of count of letters found in string, multiplied by the letter frequency.
englishScore :: BS.ByteString -> Double
englishScore sentence = letterScoreTotal - (3 * wordsCount) + nonLetterScore
	where 
		-- Computes total score for all letters by adding up the score for each letter.
		letterScoreTotal = M.foldrWithKey letterScoreHelper 0 relativeFrequency

		-- Counts the number of words in the sentence.
		wordsCount = fromIntegral $ length $ BS.split (BS.c2w ' ') sentence
		
		-- Computes the score for one English letter (no non-letters).
		letterScoreHelper char relFrequency acc = if relFrequency > 0
			then acc + sqrt ((relFrequency - (englishLetterFrequency char)) ** 2)
			else acc
		
		-- Computes the score for each non-letter symbol.
		nonLetterScore = M.foldrWithKey (\char count acc -> acc + (nonEnglishLetterScore char (M.lookup char englishLetterFrequencyMap)) * count) 0 stringLetterFrequency
		
		-- Computes the relative frequency of a letter in a sentence.
		relativeFrequency =	M.foldrWithKey (\letter count acc -> M.insert letter (100.0 * count / letterCount) acc) M.empty stringLetterFrequency

		-- Counts the number of letters in a string, and their count.
		stringLetterFrequency = computeStringLetterFrequency sentence

		-- Given a letter, returns the score for how probable that letter is to appear in the English language, with small adjustments.
		englishLetterFrequency l = letterScoreAdditionalAdjustments (lowerCaseLetter l) (M.lookup l englishLetterFrequencyMap)
		
		letterCount = fromIntegral $ BS.length sentence
		lowerCaseLetter l = BS.c2w $ toLower $ BS.w2c l

-- Simple constructor that returns an analyzed string with its score computed.
getEnglishScoresOfString (AnalyzedString sentence _ key) = AnalyzedString sentence (englishScore sentence) key

-- Find the AnalyzedString that has the lowest score (which means it's the most probably English sentence).
findMostProbablyEnglishSentence listOfScoresAndStrings = minimumBy (compare `on` getAnalyzedStringScore) listOfScoresAndStrings

-- The initial hex string xored with a secret key
set1Challenge3String = hexToBS "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

-- Returns an AnalyzedString that contains a string xored with the original given string, and a key of length(originalString) of the given character.
getStringXoredWithLetterKey string repeatLetter = AnalyzedString xoredString 0 createdXorKey
	where
		xoredString = xorTwoByteStrings string createdXorKey
		createdXorKey = createXorKey string repeatLetter

data AnalyzedString a = AnalyzedString {
	string :: a,
	score :: Double,
	key :: a
} deriving Show

getAnalyzedStringScore (AnalyzedString _ s _) = s
getAnalyzedStringSentence (AnalyzedString s _ _) = s
getAnalyzedStringKey (AnalyzedString _ _ s) = s

-- Given a string, tries to xor it with all homogenous character keys, and return the one that looks like English.
findOneXorEncryptedEnglishSentence string = findMostProbablyEnglishSentence listOfScoresAndStringsAndKeys
	where
		charactersToXorWith = [1..255]
		xoredStringsAndKeys = map (getStringXoredWithLetterKey string) charactersToXorWith
		listOfScoresAndStringsAndKeys = map getEnglishScoresOfString xoredStringsAndKeys

set1Challenge3 = do
	let mostProbableEnglishSentence = findOneXorEncryptedEnglishSentence set1Challenge3String
	--mapM_ (\e -> putStrLn $ show e) listOfScoresAndStrings
	putStrLn "Matasano Set 1 Challenge 3."
	putStr "Given encrypted string: "
	BSC.putStrLn set1Challenge3String
	putStr "Most probable decrypted English Sentence: "
	BSC.putStrLn $ getAnalyzedStringSentence mostProbableEnglishSentence
	putStr "Decrypted with key: "
	BSC.putStrLn $ getAnalyzedStringKey mostProbableEnglishSentence
	putStrLn ""

set1Challenge4 = do
	fileContents <- BS.readFile "4.txt"
	let strings = map hexToBS $ BS.split (BS.c2w '\n') fileContents
	let lineAnalyzedStrings = map (\s -> findOneXorEncryptedEnglishSentence s) strings
	let !finalSentence = findMostProbablyEnglishSentence lineAnalyzedStrings
	putStrLn "Matasano Set 1 Challenge 4."
	putStr "Decrypted message: " 
	BSC.putStrLn $ getAnalyzedStringSentence finalSentence
	putStr "Decrypted with key: "
	BSC.putStrLn $ getAnalyzedStringKey finalSentence
	putStrLn ""


set1Challenge5String = "Burning 'em, if you ain't quick and nimble\n\ 
\I go crazy when I hear a cymbal"
set1Challenge5Key = "ICE"

getStringXoredWithRepeatedKey message repeatString = xoredString
	where
		xoredString = xorTwoByteStrings message createdXorKey
		createdXorKey = (BS.cycle repeatString)

set1Challenge5 = do
	let encryptedMessage = B16.encode $ getStringXoredWithRepeatedKey set1Challenge5String set1Challenge5Key
	putStrLn "Matasano Set 1 Challenge 5."
	putStr "Original Mesage: " 
	BSC.putStrLn $ set1Challenge5String
	putStr "Encryption key: " 
	BSC.putStrLn $ set1Challenge5Key
	putStrLn "Encrypted Message: "
	BSC.putStrLn $ encryptedMessage
	putStrLn ""


set1Challenge6String1 = "this is a test"
set1Challenge6String2 = "wokka wokka!!!"

-- Given a ByteString, returns a list of Bool values corresponding to the set bits in the bytestring.
bitsOfByteString s = concat $ map (toListOf BL.bits) (s^.BS.unpackedBytes)

-- Computes the bit Hamming distance between two ByteStrings.
bitBSHammingDistance s1 s2 = foldr (\(a, b) acc -> acc + (if a == b then 0 else 1)) 0 newList
	where
		s1Bits = bitsOfByteString s1
		s2Bits = bitsOfByteString s2
		newList = zip s1Bits s2Bits

-- Given a ByteString and a key size, will compute the normalized edit distance
-- of the first four chunks of KeySize length.
keySizeEditDistanceNormalizer contents size = (size, (fromIntegral dist) / (fromIntegral size) / 6)
	where 
		(a, r1) = BS.splitAt size contents
		(b, r2) = BS.splitAt size r1
		(c, r3) = BS.splitAt size r2
		(d, _) = BS.splitAt size r3
		d1 = bitBSHammingDistance a b
		d2 = bitBSHammingDistance a c
		d3 = bitBSHammingDistance a d
		d4 = bitBSHammingDistance b c
		d5 = bitBSHammingDistance b d
		d6 = bitBSHammingDistance c d
		dist = d1 + d2 + d3 + d4 + d5 + d6

-- Splits a ByteString into Chunks of a given size.
chunksOfBS n string = helper n string []
	where 
		helper n (BS.uncons -> Nothing) acc = acc
		helper n ss acc = helper n (BS.drop n ss) (acc ++ [BS.take n ss]) 

-- Given a list of original ByteStrings, it creates a transposed list of ByteStrings
-- where the first ByteString is composed of the first letters of all the original strings,
-- the second ByteString is composed of the second letters of all the original strings,
-- and so on.
transposeBlocks keySize blocks = map singleBlockBuilder [0..keySize]
	where 
		singleBlockBuilder index = BB.toLazyByteString $ foldr (transposeBuilder index) mempty blocks
		transposeBuilder i string builder = if i >= BS.length string then builder else builder `mappend` (BB.word8 (BS.index string i))

-- Returns the head of a bytestring, but making sure that the bytestring is not empty.
-- In case it's empty, return a 0.
keyConstructor analyzed = headChecker
	where 
		keyString = getAnalyzedStringKey analyzed
		headChecker = if BS.null keyString then 0 else BS.head keyString

set1Challenge6 = do
	fileContents <- BS.readFile "6.txt"
	let cipher = B64.decodeLenient fileContents
	let keySizes = [2..40]
	let normalizedDistances = map (keySizeEditDistanceNormalizer cipher) keySizes
	let bestKeySize = minimumBy (compare `on` snd) normalizedDistances
	let cipherBlocks = chunksOfBS (fst bestKeySize) cipher
	let transposedBlocks = transposeBlocks (fst bestKeySize) cipherBlocks
	let lineAnalyzedStrings = map (\s -> findOneXorEncryptedEnglishSentence s) transposedBlocks
	let keyCharacters = L.delete 0 $ map keyConstructor lineAnalyzedStrings
	let finalKey = BS.pack $ keyCharacters
	let !decipheredText = getStringXoredWithRepeatedKey cipher finalKey

	putStrLn "Matasano Set 1 Challenge 6."
	putStr "Encryption key: " 
	BSC.putStrLn $ finalKey
	putStrLn "Decrypted Message: "
	BSC.putStrLn $ decipheredText
	putStrLn ""

set1Challenge7Key = "YELLOW SUBMARINE"

set1Challenge7 = do
	fileContents <- BS.readFile "7.txt"
	let cipher = B64.decodeLenient fileContents
	let strictKey = BS.toStrict set1Challenge7Key
	let plainText = AES.crypt AES.ECB strictKey strictKey AES.Decrypt cipher

	putStrLn "Matasano Set 1 Challenge 7."
	putStr "Encryption key: " 
	BSC.putStrLn $ set1Challenge7Key
	putStrLn "Decrypted Message: "
	BSC.putStrLn $ plainText
	putStrLn ""


main = do
	set1Challenge1
	set1Challenge2
	set1Challenge3
	set1Challenge4
	set1Challenge5
	set1Challenge6
	set1Challenge7








