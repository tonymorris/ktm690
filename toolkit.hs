import Data.Set(Set)
import qualified Data.Set as S

----

ktm690toolkit ::
  Items String String
ktm690toolkit =
  Items
    [
      item1 "Torx T20" "torx"
    , item1 "Torx T25" "torx"
    , item1 "Torx T27" "torx"
    , item1 "Torx T30" "torx"
    , item1 "Torx T40" "torx"
    , item1 "Torx T45" "torx"
    , item1 "Allen 4mm" "allen"
    , item1 "Allen 5mm" "allen"
    , item1 "Allen 6mm" "allen"
    , item1 "Allen 8mm" "allen"
    , item1 "Hex 6mm" "hex"
    , item1 "Hex 8mm" "hex"
    , item1 "Hex 10mm" "hex"
    , item1 "Hex 12mm" "hex"
    , item1 "Hex 13mm" "hex"
    , item1 "Hex 27mm" "hex"
    , item "Tie wire"
    , item "Cable ties"
    , item "Duct tape"
    , item "Nose pliers"
    , item "Torch"
    , item "Socket wrench"
    , item "Socket wrench extension"
    , item "Socket joint"
    , item "Fuses"
    , item "Philips screwdriver"
    , item "Blade screwdriver"
    , item "Knife"
    ]

----

data Item a t =
  Item
    a
    (Set t)
  deriving (Eq, Ord, Show)

newtype Items a t =
  Items [Item a t]  
  deriving (Eq, Ord, Show)

item ::
  a
  -> Item a t
item a =
  Item a S.empty

item1 ::
  a
  -> t
  -> Item a t
item1 a =
  Item a . S.singleton
