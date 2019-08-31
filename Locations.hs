module Locations (binLoc, libLoc, includeLoc, dataLoc) where

binLoc :: String
binLoc = "/usr/bin/"

libLoc :: String
libLoc = "./" -- "/usr/lib/emperor/"

includeLoc :: String
includeLoc = "/usr/include/emperor/"

dataLoc :: String
dataLoc = "/usr/share/emperor/"
