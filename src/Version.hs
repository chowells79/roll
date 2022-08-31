{-# Language TemplateHaskell #-}
module Version where

import qualified Paths_roll as Roll
import Data.Version (showVersion)
import qualified System.Info as Info

import Language.Haskell.TH


versionExp :: Quote m => Code m String
versionExp = [|| full ||]
  where
    ver = "roll " ++ showVersion Roll.version
    comp = Info.compilerName ++ " " ++ showVersion Info.fullCompilerVersion
    env = Info.os ++ "-" ++ Info.arch
    full = ver ++ ", compiled with " ++ comp ++ " for " ++ env
