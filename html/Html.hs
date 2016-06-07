import Prelude hiding (head)
import Control.Monad.Writer

type HTML = Writer String ()
type Tag  = [(String, String)] -> HTML -> HTML
type SimpleTag = HTML -> HTML

tag :: String -> Tag
tag name attrs body = do
  tell ("<" ++ name)
  forM_ attrs $ \(t,b) -> tell (" " ++ t ++ "=\"" ++ b ++ "\"")
  tell ">"
  body
  tell ("</" ++ name ++ ">")

simpleTag :: String -> SimpleTag
simpleTag s = tag s []

html, head, body, p :: Tag

html  = tag "html"
head  = tag "head"
body  = tag "body"
p     = tag "p"

title, h1 :: String -> HTML
title = simpleTag "title" . tell
h1    = simpleTag "h1" . tell

br :: HTML
br = tell "<br />"

site :: HTML
site = html [] $ do
  head [] $ do
    title "blub"

  body [("color", "#ddccbb")] $ do
    header
    p [] $ do
      tell "Content!" >> br
      tell "More content!"

    p [] $ do
      tell "Second paragraph."


header :: HTML
header = h1 "Header!"

main :: IO ()
main = putStr $ execWriter site
