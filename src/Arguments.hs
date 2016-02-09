module Arguments where

import Options.Applicative
import Control.Monad

import MagicGeneration 

verison = "v0.0.1"

magicOpts = magicMain <$> option auto (long "passes" <>
                                       short 'n' <>
                                       help "Input the number of passes that the magic generator should execute")

opts = subparser (command "magic" (info (helper <*> magicOpts) (progDesc "Magic Number Generation")))

argMain :: IO ()
argMain = join (execParser (info (helper <*> opts) (fullDesc <> header ("Jane " <> verison) <> progDesc "Haskell Based Chess Engine")))
