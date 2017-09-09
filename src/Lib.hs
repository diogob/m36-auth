{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
-- the Out-of-the-Tarpit example in Haskell and Project:M36
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
module Lib
    ( createSchema
    , insertSampleData
    ) where

import ProjectM36.Client
import ProjectM36.DataTypes.Primitive
import ProjectM36.Tupleable
import ProjectM36.Relation
import ProjectM36.Error
import Data.Either
import GHC.Generics
import Data.Binary
import Control.DeepSeq
import qualified Data.Text as T
import Data.Time.Calendar

import Lib.Prelude

--create various database value (atom) types
type Price = Double

type Name = T.Text

type Address = T.Text

data RoomType = Kitchen | Bathroom | LivingRoom
          deriving (Generic, Atomable, Eq, Show, Binary, NFData)

data PriceBand = Low | Medium | High | Premium
               deriving (Generic, Atomable, Eq, Show, Binary, NFData)

data AreaCode = City | Suburban | Rural
              deriving (Generic, Atomable, Eq, Show, Binary, NFData)

data SpeedBand = VeryFastBand | FastBand | MediumBand | SlowBand
               deriving (Generic, Atomable, Eq, Show, Binary, NFData)

data Property = Property {
  address :: T.Text,
  price :: Price,
  photo :: T.Text,
  dateRegistered :: Day
  }
              deriving (Generic, Eq, Show)

instance Tupleable Property

data Offer = Offer {
  offerAddress :: Address,
  offerPrice :: Price,
  offerDate :: Day,
  bidderName :: Name,
  bidderAddress :: Address,
  decisionDate :: Day,
  accepted :: Bool
  }
           deriving (Generic, Eq)

instance Tupleable Offer

data Decision = Decision {
  decAddress :: Address,
  decOfferDate :: Day, --the dec prefix is needed until OverloadedRecordFields is available
  decBidderName :: Name,
  decBidderAddress :: Address,
  decDecisionDate :: Day,
  decAccepted :: Bool
  }
  deriving (Generic, Eq)

instance Tupleable Decision

data Room = Room {
  roomAddress :: Address,
  roomName :: Name,
  width :: Double,
  breadth :: Double,
  roomType :: RoomType
  }
  deriving (Generic, Eq)

instance Tupleable Room

data Floor = Floor {
  floorAddress :: Address,
  floorRoomName :: Name,
  floorNum :: Int
  }
  deriving (Generic, Eq)

instance Tupleable Floor

data Commission = Commission {
  priceBand :: PriceBand,
  areaCode :: AreaCode,
  saleSpeed :: SpeedBand,
  commission :: Price
  } deriving (Generic, Eq)

instance Tupleable Commission

createSchema :: SessionId -> Connection -> IO ()
createSchema sessionId conn = do
  --create attributes for relvars
  let
      --create uniqueness constraints
      incDepKeys = map (uncurry databaseContextExprForUniqueKey)
                [("property", ["address"]),
                 ("offer", ["offerAddress", "offerDate", "bidderName", "bidderAddress"]),
                 ("decision", ["decAddress", "decOfferDate", "decBidderName", "decBidderAddress"]),
                 ("room", ["roomAddress", "roomName"]),
                 ("floor", ["floorAddress", "floorRoomName"]),
                 --"commision" misspelled in OotT
                 ("commission", ["priceBand", "areaCode", "saleSpeed"])
                 ]
      --create foreign key constraints
      foreignKeys = [("offer_property_fk",
                      ("offer", ["offerAddress"]),
                      ("property", ["address"])),
                     ("decision_offer_fk",
                      ("decision", ["decAddress", "decOfferDate", "decBidderName", "decBidderAddress"]),
                      ("offer", ["offerAddress", "offerDate", "bidderName", "bidderAddress"])),
                     ("room_property_fk",
                      ("room", ["roomAddress"]),
                      ("property", ["address"])),
                     ("floor_property_fk",
                      ("floor", ["floorAddress"]),
                      ("property", ["address"]))
                    ]
      incDepForeignKeys = map (\(n, a, b) -> databaseContextExprForForeignKey n a b) foreignKeys
      --define the relvars
      rvExprs = [toDefineExpr (undefined :: Property) "property",
                 toDefineExpr (undefined :: Offer) "offer",
                 toDefineExpr (undefined :: Decision) "decision",
                 toDefineExpr (undefined :: Room) "room",
                 toDefineExpr (undefined :: Floor) "floor",
                 toDefineExpr (undefined :: Commission) "commission"]
      --create the new algebraic data types
      new_adts = [toAddTypeExpr (undefined :: RoomType),
                  toAddTypeExpr (undefined :: PriceBand),
                  toAddTypeExpr (undefined :: AreaCode),
                  toAddTypeExpr (undefined :: SpeedBand)]
  --gather up and execute all database updates
  putStrLn ("load relvars" :: Text)
  _ <- handleIOErrors $ mapM (executeDatabaseContextExpr sessionId conn) (new_adts ++ rvExprs ++ incDepKeys ++ incDepForeignKeys)
  pure ()

insertSampleData :: SessionId -> Connection ->  IO ()
insertSampleData sessionId conn = do
  --insert a bunch of records
  putStrLn ("load data" :: Text)
  let properties = [Property { address = "123 Main St.",
                               price = 200000,
                               photo = "123_main.jpg",
                               dateRegistered = fromGregorian 2016 4 3},
                    Property { address = "456 Main St.",
                               price = 150000,
                               photo = "456_main.jpg",
                               dateRegistered = fromGregorian 2016 5 6}]
  insertPropertiesExpr <- handleError $ toInsertExpr properties "property"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertPropertiesExpr

  let offers = [Offer { offerAddress = "123 Main St.",
                        offerPrice = 180000,
                        offerDate = fromGregorian 2017 1 2,
                        bidderName = "Steve",
                        bidderAddress = "789 Main St.",
                        decisionDate = fromGregorian 2017 2 2,
                        accepted = False }]

  insertOffersExpr <- handleError $ toInsertExpr offers "offer"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertOffersExpr

  let rooms = [Room { roomAddress = "123 Main St.",
                      roomName = "Fabulous Kitchen",
                      width = 10,
                      breadth = 10,
                      roomType = Kitchen },
               Room { roomAddress = "123 Main St.",
                      roomName = "Clean Bathroom",
                      width = 7,
                      breadth = 5,
                      roomType = Bathroom }]

  insertRoomsExpr <- handleError $ toInsertExpr rooms "room"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertRoomsExpr

  let decisions = [Decision { decAddress = "123 Main St.",
                              decOfferDate = fromGregorian 2017 1 2,
                              decBidderName = "Steve",
                              decBidderAddress = "789 Main St.",
                              decDecisionDate = fromGregorian 2017 05 04,
                              decAccepted = False }]
  insertDecisionsExpr <- handleError $ toInsertExpr decisions "decision"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertDecisionsExpr

  let floors = [Floor { floorAddress = "123 Main St.",
                        floorRoomName = "Bathroom",
                        floorNum = 1
                      }]
  insertFloorsExpr <- handleError $ toInsertExpr floors "floor"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertFloorsExpr

  let commissions = [Commission { priceBand = Medium,
                                  areaCode = City,
                                  saleSpeed = MediumBand,
                                  commission = 10000 }]
  insertCommissionsExpr <- handleError $ toInsertExpr commissions "commission"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertCommissionsExpr

  --query some records, marshal them back to Haskell

  properties' <- handleIOError $ executeRelationalExpr sessionId conn (RelationVariable "property" ())

  props <- toList properties' >>= mapM (handleError . fromTuple) :: IO [Property]
  print props

handleError :: Either RelationalError a -> IO a
handleError eErr = case eErr of
    Left err -> print err >> panic "Died due to errors."
    Right v -> pure v

handleIOError :: IO (Either RelationalError a) -> IO a
handleIOError m = do
  e <- m
  handleError e

handleIOErrors :: IO [Either RelationalError a] -> IO [a]
handleIOErrors m = do
  eErrs <- m
  case lefts eErrs of
    [] -> pure (rights eErrs)
    errs -> handleError (Left (someErrors errs))
