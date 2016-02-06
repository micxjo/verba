{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Verba.Parser (
  readInflections,
  readDictEntries,
  readInflectionsFile,
  readDictEntriesFile
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Char (digitToInt, isDigit)

import Data.Attoparsec.Text (Parser, parseOnly, string, char, letter, digit,
                             satisfy, inClass, choice, takeTill, takeWhile1,
                             sepBy1, endOfLine, isEndOfLine, isHorizontalSpace,
                             skipMany)
import qualified Data.Attoparsec.Text as Attoparsec
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import           NLP.Verba.Types
import           Paths_verba (getDataFileName)

readCase :: Text -> Case
readCase "NOM" = Nominative
readCase "GEN" = Genitive
readCase "ACC" = Accusative
readCase "ABL" = Ablative
readCase "VOC" = Vocative
readCase "LOC" = Locative
readCase _ = OtherCase

caseP :: Parser Case
caseP =
  readCase <$> choice (map string ["X", "NOM", "GEN", "ACC", "DAT", "ABL",
                                   "VOC", "LOC"])

readGender :: Char -> Gender
readGender 'M' = Masculine
readGender 'F' = Feminine
readGender 'N' = Neuter
readGender 'C' = Common
readGender _ = OtherGender

genderP :: Parser Gender
genderP = readGender <$> satisfy (inClass "MFNCX")

readNumber :: Char -> Number
readNumber 'S' = Singular
readNumber 'P' = Plural
readNumber _ = OtherNumber

numberP :: Parser Number
numberP = readNumber <$> satisfy (inClass "SPX")

readTense :: Text -> Tense
readTense "PRES" = Present
readTense "IMPF" = Imperfect
readTense "FUT" = Future
readTense "PERF" = Perfect
readTense "PLUP" = Pluperfect
readTense "FUTP" = FuturePerfect
readTense _ = OtherTense

tenseP :: Parser Tense
tenseP = readTense <$> choice (map string ["X", "PRES", "IMPF", "FUTP", "FUT",
                                           "PERF", "PLUP"])

readVoice :: Text -> Voice
readVoice "ACTIVE" = Active
readVoice "PASSIVE" = Passive
readVoice _ = OtherVoice

voiceP :: Parser Voice
voiceP = readVoice <$> choice (map string ["X", "ACTIVE", "PASSIVE"])

readMood :: Text -> Mood
readMood "IND" = Indicative
readMood "SUB" = Subjunctive
readMood "IMP" = Imperative
readMood "INF" = Infinitive
readMood "PPL" = Participle
readMood _ = OtherMood

moodP :: Parser Mood
moodP =
  readMood <$> choice (map string ["X", "IND", "SUB", "IMP", "INF", "PPL"])

digitP :: Parser Int
digitP = digitToInt <$> digit

readPerson :: Char -> Person
readPerson '1' = First
readPerson '2' = Second
readPerson '3' = Third
readPerson _ = OtherPerson

personP :: Parser Person
personP = readPerson <$> digit

readComparison :: Text -> Comparison
readComparison "POS" = Positive
readComparison "COMP" = Comparative
readComparison "SUP" = Superlative
readComparison _ = OtherComparison

comparisonP :: Parser Comparison
comparisonP =
  readComparison <$> choice (map string ["X", "POS", "COMP", "SUPER"])

readNumeralType :: Text -> NumeralType
readNumeralType "CARD" = Cardinal
readNumeralType "ORD" = Ordinal
readNumeralType "DIST" = Distributive
readNumeralType "ADVERB" = Adverbial
readNumeralType _ = OtherNumeralType

numeralTypeP :: Parser NumeralType
numeralTypeP =
  readNumeralType <$> choice (map string ["X", "CARD", "ORD", "DIST", "ADVERB"])

sepP :: Parser ()
sepP = takeWhile1 isHorizontalSpace >> pure ()

declensionP :: Parser Declension
declensionP = do
  major <- digitP <* sepP
  minor <- digitP
  pure (major, minor)

endingP :: Parser (Int, Text, Age, Freq)
endingP = do
  stemIndex <- digitP <* sepP
  endingLen <- digitP <* sepP
  ending <- if endingLen > 0
            then takeTill isHorizontalSpace <* sepP
            else pure ""
  age <- letter <* sepP
  freq <- letter
  pure (stemIndex, ending, age, freq)

nounInfP :: Parser Inflection
nounInfP = do
  char 'N' <* sepP
  declension <- declensionP <* sepP
  kase <- caseP <* sepP
  number <- numberP <* sepP
  gender <- genderP <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection {
           _ending = ending,
           _declension = declension,
           _stemIndex = stemIndex,
           _info = NounInfo kase gender number,
           _age = age,
           _freq = freq}

verbInfP :: Parser Inflection
verbInfP = do
  char 'V' <* sepP
  declension <- declensionP <* sepP
  tense <- tenseP <* sepP
  voice <- voiceP <* sepP
  mood <- moodP <* sepP
  person <- personP <* sepP
  number <- numberP <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = declension
    , _stemIndex = stemIndex
    , _info = VerbInfo tense voice mood person number
    , _age = age
    , _freq = freq
    }

adjectiveInfP :: Parser Inflection
adjectiveInfP = do
  string "ADJ" <* sepP
  declension <- declensionP <* sepP
  kase <- caseP <* sepP
  number <- numberP <* sepP
  gender <- genderP <* sepP
  comparison <- comparisonP <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = declension
    , _stemIndex = stemIndex
    , _info = AdjectiveInfo kase gender number comparison
    , _age = age
    , _freq = freq
    }

pronounInfP :: Parser Inflection
pronounInfP = do
  string "PRON" <* sepP
  declension <- declensionP <* sepP
  kase <- caseP <* sepP
  number <- numberP <* sepP
  gender <- genderP <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = declension
    , _stemIndex = stemIndex
    , _info = PronounInfo kase gender number
    , _age = age
    , _freq = freq
    }

adverbInfP :: Parser Inflection
adverbInfP = do
  string "ADV" <* sepP
  comparison <- comparisonP <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = (0, 0)
    , _stemIndex = stemIndex
    , _info = AdverbInfo comparison
    , _age = age
    , _freq = freq
    }

prepositionInfP :: Parser Inflection
prepositionInfP = do
  string "PREP" <* sepP
  kase <- caseP <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = (0, 0)
    , _stemIndex = stemIndex
    , _info = PrepositionInfo kase
    , _age = age
    , _freq = freq
    }

conjunctionInfP :: Parser Inflection
conjunctionInfP = do
  string "CONJ" <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = (0, 0)
    , _stemIndex = stemIndex
    , _info = ConjunctionInfo
    , _age = age
    , _freq = freq
    }

interjectionInfP :: Parser Inflection
interjectionInfP = do
  string "INTERJ" <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = (0, 0)
    , _stemIndex = stemIndex
    , _info = InterjectionInfo
    , _age = age
    , _freq = freq
    }

verbParticipleInfP :: Parser Inflection
verbParticipleInfP = do
  string "VPAR" <* sepP
  declension <- declensionP <* sepP
  kase <- caseP <* sepP
  number <- numberP <* sepP
  gender <- genderP <* sepP
  tense <- tenseP <* sepP
  voice <- voiceP <* sepP
  mood <- moodP <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = declension
    , _stemIndex = stemIndex
    , _info = VerbParticipleInfo kase gender number tense voice mood
    , _age = age
    , _freq = freq
    }

supineInfP :: Parser Inflection
supineInfP = do
  string "SUPINE" <* sepP
  declension <- declensionP <* sepP
  kase <- caseP <* sepP
  number <- numberP <* sepP
  gender <- genderP <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = declension
    , _stemIndex = stemIndex
    , _info = SupineInfo kase gender number
    , _age = age
    , _freq = freq
    }

numeralInfP :: Parser Inflection
numeralInfP = do
  string "NUM" <* sepP
  declension <- declensionP <* sepP
  kase <- caseP <* sepP
  number <- numberP <* sepP
  gender <- genderP <* sepP
  form <- numeralTypeP <* sepP
  (stemIndex, ending, age, freq) <- endingP
  pure Inflection
    { _ending = ending
    , _declension = declension
    , _stemIndex = stemIndex
    , _info = NumeralInfo kase gender number form
    , _age = age
    , _freq = freq
    }

inflectionP :: Parser Inflection
inflectionP = choice [nounInfP, verbInfP, adjectiveInfP, pronounInfP,
                      adverbInfP, prepositionInfP, conjunctionInfP,
                      interjectionInfP, verbParticipleInfP, supineInfP,
                      numeralInfP]

inflectionLineP :: Parser Inflection
inflectionLineP = do
  Attoparsec.takeWhile isHorizontalSpace
  inf <- inflectionP
  takeTill isEndOfLine
  pure inf

commentP :: Parser Text
commentP = do
  Attoparsec.takeWhile isHorizontalSpace
  string "--"
  Attoparsec.takeWhile isHorizontalSpace
  takeTill isEndOfLine

blankLineP :: Parser ()
blankLineP = Attoparsec.takeWhile isHorizontalSpace *> endOfLine

skipLinesP :: Parser ()
skipLinesP = skipMany (void commentP <|> blankLineP)

inflectionsP :: Parser [Inflection]
inflectionsP = skipLinesP *> sepBy1 inflectionLineP skipLinesP

nounTypeP :: Parser NounType
nounTypeP = satisfy (inClass "XSMAGNPTLW")

readVerbType :: Text -> VerbType
readVerbType "TO_BE" = ToBe
readVerbType "TO_BEING" = ToBeing
readVerbType "GEN" = TakesGenitive
readVerbType "DAT" = TakesDative
readVerbType "ABL" = TakesAblative
readVerbType "TRANS" = Transitive
readVerbType "INTRANS" = Intransitive
readVerbType "IMPERS" = Impersonal
readVerbType "DEP" = Deponent
readVerbType "SEMIDEP" = SemiDeponent
readVerbType "PERFDEF" = PerfectDefinite
readVerbType _ = OtherVerbType

verbTypeP :: Parser VerbType
verbTypeP =
  readVerbType <$> choice (map string ["X", "TO_BEING", "TO_BE", "GEN", "DAT",
                                       "ABL", "TRANS", "INTRANS", "IMPERS",
                                       "DEP", "SEMIDEP", "PERFDEF"])

readPronounType :: Text -> PronounType
readPronounType "PERS" = Personal
readPronounType "REL" = Relative
readPronounType "REFLEX" = Reflexive
readPronounType "DEMONS" = Demonstrative
readPronounType "INTERR" = Interrogative
readPronounType "INDEF" = Indefinite
readPronounType "ADJECT" = Adjectival
readPronounType _ = OtherPronounType

pronounTypeP :: Parser PronounType
pronounTypeP =
  readPronounType <$> choice (map string ["X", "PERS", "REL", "REFLEX",
                                          "DEMONS", "INTERR", "INDEF",
                                          "ADJECT"])

tagsP :: Parser [Char]
tagsP = do
  t0 <- letter <* sepP
  t1 <- letter <* sepP
  t2 <- letter <* sepP
  t3 <- letter <* sepP
  t4 <- letter
  pure [t0, t1, t2, t3, t4]

stemsP :: Parser [Text]
stemsP = do
  s0 <- Attoparsec.take 19
  s1 <- Attoparsec.take 19
  s2 <- Attoparsec.take 19
  s3 <- Attoparsec.take 19
  pure (map T.strip [s0, s1, s2, s3])

definitionP :: Parser Text
definitionP = takeTill isEndOfLine

nounSubP :: Parser (Declension, SubEntry)
nounSubP = do
  char 'N' <* sepP
  declension <- declensionP <* sepP
  gender <- genderP <* sepP
  nt <- nounTypeP
  pure (declension, NounEntry gender nt)

verbSubP :: Parser (Declension, SubEntry)
verbSubP = do
  char 'V' <* sepP
  declension <- declensionP <* sepP
  vt <- verbTypeP
  pure (declension, VerbEntry vt)

adjectiveSubP :: Parser (Declension, SubEntry)
adjectiveSubP = do
  string "ADJ" <* sepP
  declension <- declensionP <* sepP
  comparison <- comparisonP
  pure (declension, AdjectiveEntry comparison)

pronounSubP :: Parser (Declension, SubEntry)
pronounSubP = do
  string "PRON" <* sepP
  declension <- declensionP <* sepP
  pt <- pronounTypeP
  pure (declension, PronounEntry pt)

adverbSubP :: Parser (Declension, SubEntry)
adverbSubP = do
  string "ADV" <* sepP
  comparison <- comparisonP
  pure ((0, 0), AdverbEntry comparison)

prepositionSubP :: Parser (Declension, SubEntry)
prepositionSubP = do
  string "PREP" <* sepP
  kase <- caseP
  pure ((0, 0), PrepositionEntry kase)

conjunctionSubP :: Parser (Declension, SubEntry)
conjunctionSubP = string "CONJ" >> pure ((0, 0), ConjunctionEntry)

interjectionSubP :: Parser (Declension, SubEntry)
interjectionSubP = string "INTERJ" >> pure ((0, 0), InterjectionEntry)

numeralSubP :: Parser (Declension, SubEntry)
numeralSubP = do
  string "NUM" <* sepP
  declension <- declensionP <* sepP
  nf <- numeralTypeP <* sepP
  _num <- Attoparsec.takeWhile isDigit
  pure (declension, NumeralEntry nf)

packSubP :: Parser (Declension, SubEntry)
packSubP = do
  string "PACK" <* sepP
  declension <- declensionP <* sepP
  pt <- pronounTypeP
  pure (declension, PackEntry pt)

subEntryP :: Parser (Declension, SubEntry)
subEntryP = choice [nounSubP, verbSubP, adjectiveSubP, pronounSubP,
                    adverbSubP, prepositionSubP, conjunctionSubP,
                    interjectionSubP, numeralSubP, packSubP]

dictEntryP :: Parser DictEntry
dictEntryP = do
  stems <- stemsP
  (declension, sub) <- subEntryP <* sepP
  tags <- tagsP <* sepP
  definition <- definitionP
  pure DictEntry
    { _stems = stems
    , _entryDeclension = declension
    , _tags = tags
    , _definition = definition
    , _sub = sub
    }

dictEntriesP :: Parser [DictEntry]
dictEntriesP = sepBy1 dictEntryP endOfLine

-- | Parse a list of inflections from the format used by
-- Whitaker's Words program.
readInflections :: Text -> Either String [Inflection]
readInflections = parseOnly inflectionsP

-- | Parse a list of dictionary entries from the format used by
-- Whitaker's Words program.
readDictEntries :: Text -> Either String [DictEntry]
readDictEntries = parseOnly dictEntriesP

-- | Parse the standard inflections file shipped with Verba.
readInflectionsFile :: IO (Either String [Inflection])
readInflectionsFile = do
  path <- getDataFileName "data/inflects.lat"
  text <- T.IO.readFile path
  return (readInflections text)

-- | Parse the standard dictionary file shipped with Verba.
readDictEntriesFile :: IO (Either String [DictEntry])
readDictEntriesFile = do
  path <- getDataFileName "data/dictline.gen"
  text <- T.IO.readFile path
  return (readDictEntries text)
