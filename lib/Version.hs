{-# Language TemplateHaskell #-}
module Version where

import qualified Paths_roll as Roll
import Data.Version (showVersion)
import qualified System.Info as Info

-- Compiling this via template haskell to ensure the version string
-- is embedded as a literal in the binary.
versionStr :: String
versionStr = $$(
    let
        ver = "roll " ++ showVersion Roll.version
        comp = Info.compilerName ++ " " ++ showVersion Info.fullCompilerVersion
        env = Info.os ++ "-" ++ Info.arch
        full = ver ++ ", compiled with " ++ comp ++ " for " ++ env
    in
        [|| full ||]
    )
