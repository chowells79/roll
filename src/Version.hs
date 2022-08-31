module Version where

import qualified Paths_roll as Roll
import Data.Version (showVersion)
import qualified System.Info as Info


versionStr :: String
versionStr = ver ++ ", compiled with " ++ comp ++ " for " ++ env
  where
    ver = "roll " ++ showVersion Roll.version
    comp = Info.compilerName ++ " " ++ showVersion Info.fullCompilerVersion
    env = Info.os ++ "-" ++ Info.arch
