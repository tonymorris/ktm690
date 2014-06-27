import Data.Set(Set)

data Parts =
  Parts
    [Part]
  deriving (Eq, Ord, Show)

data Part =
  Part
    String -- name
    String -- brand
    String -- vendor
    String -- URL
    Price
    Tags
  deriving (Eq, Ord, Show)

data Tags =
  Tags
    (Set String)
  deriving (Eq, Ord, Show)

data Price =
  Price
    Int -- cents
    Currency
  deriving (Eq, Ord, Show)
  
data Currency =
  USD
  | AUD
  deriving (Eq, Ord, Show)
