{-# LANGUAGE GADTs #-}

module NameCoin.Domain where

import Data.Map (Map)
import Data.ByteString (ByteString)

import Network.Info

data Domain = Domain
  { _ip  :: [IPv4]
  , _ip6 :: [IPv6]
  , _aliases :: [String]
  , _email :: String
  , _info :: String
  , _loc :: X
  , _services :: Map String X
  , _import :: [Name Domain]
  , _freenet :: String
  , _cjdns :: String
  , _data :: ByteString
  , _version :: Integer
  }

data X = X -- TODO in spec

data Name a where
  DName :: String -> Name Domain
