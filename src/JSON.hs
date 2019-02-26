--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 22: Fun with sequential composition                                --
--------------------------------------------------------------------------------

module JSON where

--------------------------------------------------------------------------------

import Lab7 -- solutions to Lab7 are needed

--------------------------------------------------------------------------------
-- JSON representation

type Object = [(String, Value)]
type Array  = [Value]

data Value = Obj Object
           | Arr Array
           | Str String
           | Num Integer
           | Bool Bool
           | Null
           deriving Show

--------------------------------------------------------------------------------
-- JSON parsing

-- Reminder:
-- instance Functor Parser
-- instance Applicative Parser
-- instance Monad Parser



--------------------------------------------------------------------------------
