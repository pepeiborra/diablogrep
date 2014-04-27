{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types where
import Data.Aeson.TH
import Data.Foldable (Foldable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable
import Text.Printf
import Prelude hiding (id,mapM, mapM_, min, max)

data Career = Career {heroes :: [Hero]}             deriving (Eq,Show,Ord)
data Hero   = Hero {name :: String, id :: Integer}  deriving (Eq,Show,Ord)
data Item   = Item { attributesRaw :: AttributeMap, gems :: [Gem]} deriving (Eq,Ord)
data Gem    = Gem { gem_attributesRaw :: AttributeMap } deriving (Eq,Ord)
type AttributeMap = Map String Attribute
data Attribute = Attr {min :: Double, max :: Double} deriving (Eq,Show,Ord)
data HeroDetails item = HeroD { items :: ItemSet item } deriving (Eq,Show,Ord,Functor,Foldable,Traversable)
data ItemSet item = ItemSet { neck, waist, feet, bracers, torso, hands, head, legs, leftFinger, rightFinger, shoulders, offHand :: item} deriving (Eq,Show,Ord,Functor,Foldable,Traversable)
data ItemRef = ItemRef {tooltipParams :: String} deriving (Eq,Show,Ord)

$(deriveJSON defaultOptions ''Career)
$(deriveJSON defaultOptions ''Hero)
$(deriveJSON defaultOptions ''HeroDetails)
$(deriveJSON defaultOptions ''Attribute)
$(deriveJSON defaultOptions ''Item)
$(deriveJSON defaultOptions ''ItemRef)
$(deriveJSON defaultOptions ''ItemSet)
$(deriveJSON defaultOptions{ fieldLabelModifier = drop 4 } ''Gem)

instance Show Item where
  show (Item attrs _) = unlines [ printf "%s: %s" name (if min == max then show min else show (min,max)) | (name,Attr{min,max}) <- Map.toList attrs]

instance Num Attribute where
  a + b = Attr (min a + min b) (max a + max b)
  fromInteger x = Attr (fromInteger x) (fromInteger x)
