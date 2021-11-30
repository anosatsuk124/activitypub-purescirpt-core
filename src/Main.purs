module Main where

import HTTPure
import HTTPure.Lookup
import Prelude

import Data.Maybe (Maybe(..))
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import HTTPure.Headers (header)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)

config = { preferredUsername: "a", name: "Alice" }

private_key :: Maybe String
private_key = Nothing
public_key :: Maybe String
public_key= Nothing

read :: FilePath -> String
read path = unsafePerformEffect $ readTextFile UTF8 path

main :: ServerM
main = serve 8080 router $ log "Server now up on port 8080"
  where
    router :: Request -> ResponseM
    router { path }
      | path !@ 0 == "" = ok' (header "text" "plain") "hello, world"
      | path !@ 0 == "public" = ok' (header "text" "html") (read "./public/index.html")
      | path !@ 0 == "u" && path !@ 1 == config.preferredUsername = ok' (header "application" "activity+json") (read "./activity.json")
      | path !@ 0 == "u" = notFound
      | otherwise = notFound
