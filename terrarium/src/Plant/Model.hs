module Plant.Model where

type TaxonomyName = String
type TaxonomyHierarchy = Integer
type PlantCommonName = String
type PlantScientificName = String

data Taxonomy = Taxonomy    { taxonomyName :: TaxonomyName
                            , taxonomyHiearchy :: TaxonomyHierarchy }

data Plant = Plant          { plantCommonName :: PlantCommonName
                            , plantScientificName :: PlantScientificName
                            , plantParent :: Maybe Plant
                            , plantTaxonomy :: Maybe Taxonomy
                            , plantAccepted :: Bool
                            , plantUnacceptedReason :: Maybe String
                            , plantPublic :: Bool }
