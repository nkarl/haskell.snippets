module Library.Monads.State where

import Control.Monad.State

indentNewLine :: String -> String -> State Int String
indentNewLine msg pMsg = do
    indentLevel <- get
    let
        nextIndentLevel = indentLevel + 2
        indent = replicate nextIndentLevel ' '
        output = pMsg <> indent <> msg <> "\n"
    put nextIndentLevel
    pure output

demo :: IO ()
demo =
    putStrLn $ evalState msg 0
  where
    msg =
        indentNewLine "hello" ""
            >>= indentNewLine "world"
            >>= indentNewLine "love, "
            >>= indentNewLine "George"
