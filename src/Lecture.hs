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
e0 = "{\"id\": 59, \"title\": \"End of term Meal\", \"when\": \"2020-03-11T17:45:00Z\", \"where\": \"Shin Ramen\" }"

-- e0v :: Maybe Value
-- e0v = fst <$> parse valP e0
--
-- e0e :: Maybe Event
-- e0e = e0v >>= parseJSON

e1 :: String
e1 = "{\"id\": 60, \"title\": \"Old School Anime Night\", \"when\": \"2020-02-29T16:00:00Z\", \"where\": \"MS.05\" }"

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
