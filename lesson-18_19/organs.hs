import qualified Data.Map as Map

data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq, Ord, Enum)

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip organsEnum organCounts)
  where
    organCounts = map getOrganCount organsEnum
    getOrganCount = (\organ -> length (filter (\x -> x == organ) organs))
    organsEnum = (enumFrom (toEnum 0))
