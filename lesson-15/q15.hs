data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where
        halfAlphabet = alphabetSize `div` 2
        offset       = fromEnum c + halfAlphabet
        rotation     = offset `mod` alphabetSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where
        halfN    = n `div` 2
        offset   = if even n then fromEnum c + halfN else 1 + fromEnum c + halfN
        rotation = offset `mod` n

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
        where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
    where
        alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4l     = rotN alphaSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
    where
        alphaSize = 1 + fromEnum (maxBound :: Char)
        rotChar   = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
    where
        alphaSize      = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNdecoder alphaSize

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
        then False : intToBits' nexVal
        else True : intToBits' nexVal
    where
        remainder = n `mod` 2
        nexVal    = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where
        reversedBits  = reverse (intToBits' n)
        missingBits   = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
    where
        size          = length bits
        indices       = [size - 1, size - 2 .. 0]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> (fst pair) `xor` (snd pair))
                              (zip padBits plaintextBits)
    where
        padBits       = map charToBits pad
        plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
        where bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
        encode Rot text = rotEncoder text
        decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
        encode (OTP pad) text = applyOTP pad text
        decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

exampleOTP :: OneTimePad
exampleOTP = OTP
        (cycle
                (map
                        (\x -> toEnum x)
                        (map (\x -> examplePRNG x)
                             [minBound :: Int .. maxBound :: Int]
                        )
                )
        )

data StreamCipher = StreamCipher

instance Cipher StreamCipher where
        encode text = applyOTP exampleOTP text
        decode text = applyOTP exampleOTP text

