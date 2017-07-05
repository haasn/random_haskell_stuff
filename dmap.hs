{-# LANGUAGE GADTs #-}

import Data.Functor
import Data.Semigroup
import Data.Dependent.Map as D
import Data.GADT.Compare

data AudioFilter = AF { name :: String }
    deriving Show

-- Dependent option GADTs with tagged implementation types and reified
-- semigroup instances

data Opt k where
    Volume   :: Opt (Last Int)
    Mute     :: Opt (Last Bool)
    Filters  :: Opt [AudioFilter]
    Whatever :: Opt (Sum Double)
    -- ...

-- Comparison boilerplate, TODO: find a way to generate this instance
instance GCompare Opt where
    gcompare Volume  Volume  = GEQ
    gcompare Volume  _       = GLT

    gcompare Mute    Volume  = GGT
    gcompare Mute    Mute    = GEQ
    gcompare Mute    _       = GLT

    gcompare Filters Volume  = GGT
    gcompare Filters Mute    = GGT
    gcompare Filters Filters = GEQ
    gcompare Filters _       = GLT

    gcompare Whatever Whatever = GEQ
    gcompare Whatever _        = GGT

instance GEq Opt where
    geq a b = case gcompare a b of
        GEQ -> Just Refl
        _   -> Nothing

data IsSemigroup v where
    IsSemigroup :: Semigroup t => t -> IsSemigroup t

mapSemi :: (a -> a) -> IsSemigroup a -> IsSemigroup a
mapSemi f (IsSemigroup a) = IsSemigroup (f a)

-- Dependent map helpers

type Profile = DMap Opt IsSemigroup

(~>) :: Semigroup a => Opt a -> a -> DSum Opt IsSemigroup
k ~> v = k :=> IsSemigroup v

lookupOpt :: Opt a -> Profile -> Maybe a
lookupOpt k = fmap (\(IsSemigroup v) -> v) . D.lookup k

-- Example profiles, assume these were produced by parsing the config file
-- instead

globalDefault :: Profile
globalDefault = fromList
    [ Volume   ~> Last 100
    , Mute     ~> Last False
    , Filters  ~> []
    , Whatever ~> Sum 0
    ]

profileA :: Profile
profileA = fromList [ Volume  ~> Last 50
                    , Filters ~> [ AF "loudnorm" ]
                    ]

profileB :: Profile
profileB = fromList [ Filters  ~> [ AF "distort", AF "enhance" ]
                    , Whatever ~> Sum 0.5
                    ]

commandLine :: Profile
commandLine = fromList [ Volume   ~> Last 80
                       , Whatever ~> Sum 3.0
                       ]

-- Profile stack

type ProfileStack = [Profile]

-- since this is a top-down stack, profiles are merged right-to-left
mergeStack :: ProfileStack -> Profile
mergeStack = unionsWithKey $ \_ (IsSemigroup a) (IsSemigroup b)
                             -> IsSemigroup (b <> a)

example :: ProfileStack
example = [ commandLine, profileA, profileB, globalDefault ]

-- XXX: these error messages could be avoided with a slightly smarter
-- construction to enforce this property, but it doesn't really matter for
-- sake of example

getOpt :: Opt a -> ProfileStack -> a
getOpt k ps = case lookupOpt k (mergeStack ps) of
    Just a -> a
    Nothing -> error "Missing default value for option!"

modifyOpt :: (a -> a) -> Opt a -> ProfileStack -> ProfileStack
modifyOpt f k (p:ps)
    | member k p = adjust (mapSemi f) k p : ps
    | otherwise  = p : modifyOpt f k ps


{- Usage example:

[1 of 1] Compiling Main             ( dmap.hs, interpreted )
Ok, modules loaded: Main.

λ getOpt Filters example
[AF {name = "distort"},AF {name = "enhance"},AF {name = "loudnorm"}]
λ getOpt Volume example
Last {getLast = 80}
λ getOpt Whatever example
Sum {getSum = 3.5}

λ Prelude.map (lookupOpt Volume) example
[Just (Last {getLast = 80}),Just (Last {getLast = 50}),Nothing,Just (Last {getLast = 100})]

λ Prelude.map (lookupOpt Volume) $ modifyOpt (fmap (+5)) Volume example
[Just (Last {getLast = 85}),Just (Last {getLast = 50}),Nothing,Just (Last {getLast = 100})]

As you can see, only the ‘last’ source of an option is the one that gets
modified. Disabling a profile (or option set, whatever you want to call it)
would be removing it from this list. Enabling it would be re-inserting it at
the head of this list.

-}
