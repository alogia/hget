module Types where

import Text.Regex
import Text.Regex.Posix

soundExt = ["mp3", "mp4", "ogg", "wma", "flac"]
imageExt = ["jpe?g", "png", "tiff?"]
videoExt = [ "avi", "flv", "mkv", "mov"]

mkExtRegex exts = map (\l -> mkRegexWithOpts ("\\." ++ l ++ "$") False True) exts

allMediaRegex = mkExtRegex $ soundExt ++ imageExt ++ videoExt
