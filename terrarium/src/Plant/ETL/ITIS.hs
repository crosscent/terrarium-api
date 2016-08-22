{-# LANGUAGE OverloadedStrings #-}
module Plant.ETL.ITIS where

import Data.Foldable                                (forM_)
import Data.Maybe                                   ( fromMaybe )
import Control.Monad.IO.Class                       ( liftIO )
import qualified Data.Conduit
import qualified Data.Conduit.List                  ( map
                                                    , mapM_
                                                    , sourceList )

import qualified Data.Text                          ( unpack
                                                    , Text )
import qualified Database.SQLite.Simple             ( execute
                                                    , open
                                                    , query_
                                                    , Connection
                                                    , Only (..) )
import qualified Database.SQLite.Simple.FromRow

-- please place ITIS.sqlite in /extras/

type Plant = (Data.Text.Text, Int, Data.Text.Text, Maybe Data.Text.Text, Maybe Data.Text.Text, Maybe Int, Maybe Data.Text.Text)
type Taxonomy = (Int, Data.Text.Text)

plantDB :: IO Database.SQLite.Simple.Connection
plantDB = Database.SQLite.Simple.open "extras/ITIS.sqlite"

queryTaxonomy :: IO [Taxonomy]
queryTaxonomy = do
    conn <- plantDB
    Database.SQLite.Simple.query_ conn "SELECT rank_id, rank_name FROM taxon_unit_types WHERE kingdom_id=3"

queryPlant :: IO [Plant]
queryPlant = do
    conn <- plantDB
    plants <- Database.SQLite.Simple.query_ conn "SELECT s.complete_name, s.rank_id,\
                                                       \ s.name_usage , s.unaccept_reason,\
                                                       \ p.complete_name, p.rank_id,\
                                                       \ p.name_usage\
                                                       \ FROM taxonomic_units s\
                                                       \ LEFT JOIN taxonomic_units p\
                                                            \ ON s.parent_tsn = p.tsn\
                                                       \ WHERE s.kingdom_id=3\
                                                       \ ORDER BY s.tsn ASC" 
    return plants

sourcePlant :: Data.Conduit.Source IO Plant
sourcePlant = liftIO queryPlant >>= Data.Conduit.List.sourceList

sourceTaxonomy :: Data.Conduit.Source IO Taxonomy
sourceTaxonomy = liftIO queryTaxonomy >>= Data.Conduit.List.sourceList

conduitPlant :: Data.Conduit.Conduit Plant IO String
conduitPlant = Data.Conduit.List.map show

conduitTaxonomy :: Data.Conduit.Conduit Taxonomy IO String
conduitTaxonomy = Data.Conduit.List.map show

sink :: Data.Conduit.Sink String IO ()
sink = Data.Conduit.List.mapM_ putStrLn

listPlants :: IO ()
listPlants = do
    sourcePlant Data.Conduit.$$ conduitPlant Data.Conduit.=$ sink

listTaxonomies :: IO ()
listTaxonomies = do
    sourceTaxonomy Data.Conduit.$$ conduitTaxonomy Data.Conduit.=$ sink
