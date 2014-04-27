{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Diablo(career,hero,item,generate,run) where
import Control.Applicative
import Lens.Family  ((^.))
import Control.Monad (mplus)
import Data.Aeson
import Data.Aeson.Lens
import Data.List (intersperse)
import Data.Map ((!), fromList, Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Traversable
import Network.Wreq
import Prelude hiding (id,mapM, mapM_, min, max)
import Types

run :: ((?host::String, ?battletag::String, ?char::String) => a) -> a
run m = let ?host="eu"
            ?battletag="recursive-2195"
            ?char="Idiom"
          in m

main = run generate

retrieve url = do
  contents <- asJSON =<< get url
  return (contents ^. responseBody)

careerUrl = concat ["http://",?host,".battle.net/api/d3/profile/",?battletag,"/"]
career = retrieve careerUrl :: IO Career

heroUrl = do
  career <- career
  let characters = fromList [ (name c, id c) | c <- heroes career ]
  let hero = concat["http://",?host,".battle.net/api/d3/profile/",?battletag,"/hero/",(show$ characters ! ?char)]
  return hero
hero = do
  hero <- heroUrl >>= retrieve :: IO (HeroDetails ItemRef)
  mapM item hero

itemUrl id = concat ["http://", ?host, ".battle.net/api/d3/data/item/", id]
item (ItemRef id) = retrieve (itemUrl (drop 5 id)) :: IO Item

tabulateItem :: Item -> String
tabulateItem it =
  concat $ intersperse "\t" [ maybe "" (show.max) (k it) | k <- table ]

generate = (fmap.fmap) tabulateItem hero

orElse m1 m2 it = m1 it `mplus` m2 it
unpack tag (Item attrs gems) = do
  main <- unpackAttrs attrs
  let extra = map (unpackAttrs . gem_attributesRaw) gems
  return $ sum (main : catMaybes extra)
 where
  unpackAttrs attrs = Map.lookup tag attrs

table =
  [ unpack "Damage_Min#Physical"
  , \it@(Item attrs _) -> do { min <- (table!!0) it
                                            ; delta <- Map.lookup "Damage_Delta#Physical" attrs
                                            ; return ( min + delta ) }
  , unpack "Intelligence_Item"
  , unpack "Crit_Percent_Bonus_Capped"
  , unpack "Crit_Damage_Percent"
  , unpack "Attacks_Per_Second_Percent"
  , unpack "Vitality_Item"
  , unpack "Hitpoints_Max_Percent_Bonus_Item"
  , unpack "Resistance_All"
  , unpack "Armor_Item" `orElse` unpack "Armor_Bonus_Item"
  , unpack "Damage_Percent_Bonus_Vs_Elites"
  , unpack "Damage_Dealt_Percent_Bonus#Arcane"
  , unpack "Damage_Dealt_Percent_Bonus#Fire"
  , unpack "Damage_Dealt_Percent_Bonus#Lightning"
  , unpack "Damage_Dealt_Percent_Bonus#Cold"
  , unpack "Power_Damage_Percent_Bonus#Wizard_ArcaneTorrent"
  , unpack "Power_Damage_Percent_Bonus#Wizard_ArcaneOrb"
  , unpack "Power_Damage_Percent_Bonus#Wizard_Disintegrate"
  , unpack "Power_Damage_Percent_Bonus#Wizard_WaveOfForce"
  , unpack "Power_Damage_Percent_Bonus#Wizard_Meteor"
  , unpack "Power_Damage_Percent_Bonus#Wizard_ExplosionBlast"
  ]
