-- MPD Browser version 0.1
-- Author: nand
-- License: GPL

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Network.MPD

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Exception

import System.Locale.SetLocale
import Data.Char.WCWidth
import Data.Maybe (fromJust)
import qualified Data.Map as M

type HsMpc a  = StateT FolderView IO a

data FolderView = FV
  { top :: [LsResult]
  , rest :: [LsResult]
  , parent :: Maybe FolderView
  }

main :: IO ()
main = do
  setLocale LC_ALL $ Just ""
  --start
  initCurses


  cBreak True
  echo False
  keypad stdScr True
  startColor
  cursSet CursorInvisible

  (initialView >>= runStateT loop >> return ()) `finally` endWin


initialView :: IO FolderView
initialView = do
  list <- tryResult $ lsInfo "" -- "" is the root path in MPD
  return $ FV [] list Nothing


loop :: HsMpc ()
loop = do
  -- Output the list of found files
  get >>= liftIO . output

  -- Wait for a keypress
  liftIO getCh >>= handle

  where
    handle :: Key -> HsMpc ()
    -- Move up or down and repeat
    handle KeyUp   = modify moveUp >> loop
    handle KeyDown = modify moveDown >> loop

    -- Interact with folders/files
    handle (KeyChar '\r') = handle (KeyChar '\n')
    handle (KeyChar '\n') = get >>= liftIO . enter >>= put >> loop
    handle (KeyChar ' ')  = get >>= liftIO . append >> modify moveDown >> loop
    handle KeyBackspace   = modify up >> loop

    -- 'q' exits the application
    handle (KeyChar 'q') = return ()

    -- Just redraw with the new dimensions
    handle KeyResize = loop

    -- Some aliases
    handle (KeyChar 'j') = handle KeyDown
    handle (KeyChar 'k') = handle KeyUp

    -- Otherwise, just handle another keypress
    handle _ = liftIO getCh >>= handle


-- Move the cursor up or down
moveUp :: FolderView -> FolderView
moveUp (FV (t:ts) r h) = FV ts (t:r) h
moveUp v@(FV [] _ _)   = v

-- The extra b:bs is to ensure that the bottom list can never be empty
moveDown :: FolderView -> FolderView
moveDown (FV t (c:b:bs) h) = FV (c:t) (b:bs) h
moveDown v@(FV _ _ _)   = v


-- Enter a folder and/or play back a file
enter :: FolderView -> IO FolderView
enter v@(FV _ [] _)    = return v -- Empty folder
enter v@(FV t (c:r) p) = case c of
  Song s -> do
    case p of
      -- Determine whether or not we're currently in a playlist view
      Just (FV _ (LsPlaylist l:_) _) -> do
        songid <- appendPlaylistItem l (length t)
        withMPD $ moveId songid (-1) >> playId songid

      -- Not in a playlist
      _ -> withMPD $ playId  =<< addId (sgFilePath s) (Just (-1))

    return v

  LsDirectory s  -> do
    contents <- tryResult $ lsInfo s
    return $ FV [] contents (Just v)

  LsPlaylist s -> do
    contents <- tryResult $ listPlaylistInfo s
    return $ FV [] (map Song contents) (Just v)

-- Go back / up a folder
up :: FolderView -> FolderView
up v@(FV _ _ Nothing) = v
up (FV _ _ (Just v))  = v

-- Append a file or folder to the playlist
append :: FolderView -> IO ()
append (FV _ [] _)    = return ()
append (FV t (c:_) p) = case p of
  -- Determine whether or not song is in a playlist
  Just (FV _ (LsPlaylist l:_) _) -> do
    appendPlaylistItem l $ length t
    return ()

  -- Not in a play list
  _ -> do
    case c of
      (LsPlaylist s)  -> withMPD . load $ s
      (Song s)      -> withMPD . add $ sgFilePath s
      (LsDirectory s) -> withMPD . add $ s
    return ()

-- Append a single item from a playlist and return its id
appendPlaylistItem :: Path -> Int -> IO Id
appendPlaylistItem list index = do
  current <- tryResult $ playlistInfo Nothing
  withMPD $ load list
  new <- tryResult $ playlistInfo Nothing

  let (first, this:rest) = splitAt index $ drop (length current) new
  withMPD . mapM_ (deleteId . fromJust . sgId) $ first ++ rest

  return . fromJust . sgId $ this


---- Display functions ----

-- Output a FolderView
output :: FolderView -> IO ()
output fv = do
  (h, w) <- scrSize
  --wclear stdScr
  gotoTop

  let maxSize = (h-1) `div` 2
      top'    = take maxSize $ top fv
      topLen  = length top'

  -- Output the newlines if necessary
  replicateM_ (maxSize - topLen) addLn

  -- Output the top in reverse order
  mapM_ (wAddStr stdScr . format w) $ reverse top'

  case rest fv of
    -- Make sure the list is non-empty
    (cur:bot) -> do

      -- Current item has reversed colors
      attrReverseOn
      wAddStr stdScr $ format w cur

      attrReverseOff
      mapM_ (wAddStr stdScr) . take maxSize $ map (format w)  bot ++ Prelude.repeat "\n"

    -- Otherwise, fill up regardless
    _         -> mapM_ (wAddStr stdScr) . take maxSize $ Prelude.repeat "\n"

  refresh

  where
    format :: Int -> LsResult -> String
    format w (LsDirectory path)  = '[' : takew (w-3) (strip path) ++ "]\n"
    format w (Song song) = makew w song
    format w (LsPlaylist path) = '(' : takew (w-3) (strip path) ++ ")\n"

    -- Strip all of the leading path name up to the last /\\\
    strip :: Path -> Path
    strip = reverse . takeWhile (/='/') . reverse

    -- Format into a certain width, returns the resulting length
    takew' :: Int -> String -> (Int, String)
    takew' _ [] = (0, [])
    takew' n (x:xs)
      | n > n'    = (n' + fst x', x : snd x')
      | otherwise = (1, "\x2026")
        where n'  = wcwidth x
              x'  = takew' (n - n') xs

    -- Like the above but ignore the resulting length
    takew :: Int -> String -> String
    takew w = snd . takew' w

    -- Format a song correctly
    makew :: Int -> Song -> String
    makew w song =
      let l  = formatLength $ sgLength song
          ll = length l
          (sl,sn) = takew' (w - ll) s

          tags = sgTags song
          s  = case (M.lookup Artist tags, M.lookup Title tags) of
            (Nothing, Nothing) -> strip $ sgFilePath song

            -- If the list is empty the tag is already non-present
            (Just a, Just t)   -> head a ++ " - " ++ head t
            (Nothing, Just t)  -> head t
            (Just a, Nothing)  -> head a ++ " - {" ++ strip (sgFilePath song) ++ "}"
        in
          -- Fill up as many whitespace after the song name as needed to make the width correct
          sn ++ replicate (w - ll - sl) ' ' ++ l

    formatLength :: Seconds -> String
    formatLength l
      | l > 3600  = concat [" (", show h, ":", show m, ":", show s, ")"]
      | otherwise = concat [" (", show m, ":", show s, ")"]

      where h  = l  `div` 3600
            s' = l  `mod` 3600
            m  = s' `div` 60
            s  = s' `mod` 60


---- Auxiliary functions ----

-- Try and get a result, fail with [] if any MPDError occured
tryResult :: MPD [a] -> IO [a]
tryResult m = do
  response <- withMPD m

  case response of
    Left _  -> return []
    Right l -> return l


-- So apparently it's now my job to write HSCurses, too?
attrReverseOn :: IO ()
attrReverseOn = wAttrOn stdScr attrReverse

attrReverseOff :: IO ()
attrReverseOff = wAttrOff stdScr attrReverse

attrReverse :: Int
attrReverse = 262144
