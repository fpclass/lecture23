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

keyword :: String -> Parser String
keyword [] = return []
keyword (x:xs) = do
    y <- ch (==x)
    ys <- keyword xs
    return (y:ys)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = go <|> pure []
    where go = do
            r <- p
            rs <- many (sep *> p)
            return (r:rs)

nullP :: Parser Value
nullP = do
    keyword "null"
    return $ Null

trueP :: Parser Value
trueP = do
    keyword "true"
    return $ Bool True

falseP :: Parser Value
falseP = do
    keyword "false"
    return $ Bool False

boolP :: Parser Value
boolP = trueP <|> falseP

natP :: Parser Value
natP = Num <$> nat

strP :: Parser Value
strP = Str <$> between (ch (=='"')) (ch (=='"')) (many (ch (/='"')))

arrP :: Parser Value
arrP = Arr <$> between (ch (=='[')) (token $ ch (==']')) (sepBy valP (token $ ch (==',')))


valP :: Parser Value
valP = token (nullP <|> boolP <|> natP <|> strP <|> arrP)












--------------------------------------------------------------------------------
