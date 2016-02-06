{-# LANGUAGE OverloadedStrings #-}
module NLP.Verba.Types where

import Data.Text (Text)

type Declension = (Int, Int)

data Gender = Masculine
            | Feminine
            | Neuter
            | Common       -- ^ Masculine and/or feminine
            | OtherGender
            deriving (Eq, Show)

data Case = Nominative
          | Genitive
          | Accusative
          | Dative
          | Ablative
          | Vocative
          | Locative
          | OtherCase
          deriving (Eq, Show)

data Number = Singular
            | Plural
            | OtherNumber
            deriving (Eq, Show)

data Tense = Present
           | Imperfect
           | Future
           | Perfect
           | Pluperfect
           | FuturePerfect
           | OtherTense
           deriving (Eq, Show)

data Voice = Active
           | Passive
           | OtherVoice
           deriving (Eq, Show)

data Mood = Indicative
          | Subjunctive
          | Imperative
          | Infinitive
          | Participle
          | OtherMood
          deriving (Eq, Show)

data Comparison = Positive
                | Comparative
                | Superlative
                | OtherComparison
                deriving (Eq, Show)

data NumeralType = Cardinal
                 | Ordinal
                 | Distributive
                 | Adverbial
                 | OtherNumeralType
                 deriving (Eq, Show)

data PartOfSpeech = Noun
                  | Verb
                  | Adjective
                  | Pronoun
                  | Adverb
                  | Preposition
                  | Conjunction
                  | Interjection
                  | VerbParticiple
                  | Supine
                  | Numeral
                  | Pack
                  deriving (Eq, Show)

data Person = First
            | Second
            | Third
            | OtherPerson
              deriving (Eq, Show)

data InflectionInfo = NounInfo !Case !Gender !Number
                    | VerbInfo !Tense !Voice !Mood !Person !Number
                    | AdjectiveInfo !Case !Gender !Number !Comparison
                    | PronounInfo !Case !Gender !Number
                    | AdverbInfo !Comparison
                    | PrepositionInfo !Case
                    | ConjunctionInfo
                    | InterjectionInfo
                    | VerbParticipleInfo !Case !Gender !Number
                      !Tense !Voice !Mood
                    | SupineInfo !Case !Gender !Number
                    | NumeralInfo !Case !Gender !Number !NumeralType
                    deriving (Eq, Show)

type Age = Char
type Freq = Char

data Inflection = Inflection
    { _ending :: !Text
    , _declension :: !Declension
    , _stemIndex :: !Int
    , _info :: !InflectionInfo
    , _age :: !Age
    , _freq :: !Freq
    } deriving (Eq, Show)

data VerbType = ToBe           -- ^ Only the verb "to be" (esse)
              | ToBeing        -- ^ Compounds of the verb "to be" (esse)
              | TakesGenitive
              | TakesDative
              | TakesAblative
              | Transitive
              | Intransitive
              | Impersonal
              | Deponent
              | SemiDeponent
              | PerfectDefinite
              | OtherVerbType
              deriving (Eq, Show)

data PronounType = Personal
                 | Relative
                 | Reflexive
                 | Demonstrative
                 | Interrogative
                 | Indefinite
                 | Adjectival
                 | OtherPronounType
                 deriving (Eq, Show)

type NounType = Char

data SubEntry = NounEntry !Gender !NounType
              | VerbEntry !VerbType
              | AdjectiveEntry !Comparison
              | PronounEntry !PronounType
              | AdverbEntry !Comparison
              | PrepositionEntry !Case
              | ConjunctionEntry
              | InterjectionEntry
              | NumeralEntry !NumeralType
              | PackEntry !PronounType
              deriving (Eq, Show)

data DictEntry = DictEntry
    { _stems :: ![Text]
    , _entryDeclension :: !Declension
    , _tags :: ![Char]
    , _definition :: !Text
    , _sub :: !SubEntry
    } deriving (Eq, Show)

class HasPartOfSpeech a where
  partOfSpeech :: a -> PartOfSpeech

instance HasPartOfSpeech Inflection where
  partOfSpeech Inflection {_info = info} =
    case info of
      NounInfo {} -> Noun
      VerbInfo {} -> Verb
      AdjectiveInfo {} -> Adjective
      PronounInfo {} -> Pronoun
      AdverbInfo {} -> Adverb
      PrepositionInfo {} -> Preposition
      ConjunctionInfo -> Conjunction
      InterjectionInfo -> Interjection
      VerbParticipleInfo {} -> VerbParticiple
      SupineInfo {} -> Supine
      NumeralInfo {} -> Numeral

instance HasPartOfSpeech DictEntry where
  partOfSpeech DictEntry {_sub = sub} =
    case sub of
      NounEntry {} -> Noun
      VerbEntry {} -> Verb
      AdjectiveEntry {} -> Adjective
      PronounEntry {} -> Pronoun
      AdverbEntry {} -> Adverb
      PrepositionEntry {} -> Preposition
      ConjunctionEntry -> Conjunction
      InterjectionEntry -> Interjection
      NumeralEntry {} -> Numeral
      PackEntry {} -> Pack
