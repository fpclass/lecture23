--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with sequential composition                                   --
--------------------------------------------------------------------------------

module Lecture where

--------------------------------------------------------------------------------

import Lab
import JSON
import Event

--------------------------------------------------------------------------------

e0 :: String
e0 = "{\"id\": 30, \"title\": \"Karaoke\", \"when\": \"2019-02-27T18:00:00Z\", \"where\": \"S0.11\" }"

-- e0v :: Maybe Value
-- e0v = fst <$> parse valP e0
--
-- e0e :: Maybe Event
-- e0e = e0v >>= parseJSON

e1 :: String
e1 = "{\"id\": 31, \"title\": \"All Nighter\", \"when\": \"2019-03-09T20:00:00Z\", \"where\": \"B2.04\" }"

-- e1v :: Maybe Value
-- e1v = fst <$> parse valP e1
--
-- e1e :: Maybe Event
-- e1e = e1v >>= parseJSON

e2 :: String
e2 = "[" ++ e0 ++ "," ++ e1 ++ "]"

-- e2v :: Maybe Value
-- e2v = fst <$> parse valP e2
--
-- e2e :: Maybe [Event]
-- e2e = e2v >>= parseJSON

--------------------------------------------------------------------------------
