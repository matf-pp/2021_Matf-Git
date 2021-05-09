module Command.GarbageCollector (commandInit) where

import Command.Basic
import Options.Applicative

initOptions :: Parser Command
initOptions = GarbageCollector <$> optDir 

commandInit :: Mod CommandFields Command
commandInit = command "gc" (info initOptions (progDesc "Garbage Collector that removes unaccesible commits"))