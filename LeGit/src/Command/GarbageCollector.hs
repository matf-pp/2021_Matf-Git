module Command.GarbageCollector (commandGC) where

import Command.Basic
import Options.Applicative

initOptions :: Parser Command
initOptions = GarbageCollector <$> optDir 

commandGC :: Mod CommandFields Command
commandGC = command "gc" (info initOptions (progDesc "Garbage Collector that removes unaccessible commits"))