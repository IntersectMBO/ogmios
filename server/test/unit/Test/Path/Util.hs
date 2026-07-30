-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TemplateHaskell #-}

module Test.Path.Util
    ( getProjectRoot
    ) where

import Ogmios.Prelude

import Data.FileEmbed
    ( makeRelativeToProject
    )
import Language.Haskell.TH.Syntax
    ( Exp
    , Q
    , liftData
    )

import qualified System.Environment as Env
import qualified System.IO.Unsafe as Unsafe

-- | Absolute path to the directory containing the project's .cabal file.
--
-- It is resolved at compile-time, so it is only valid where (and while) the
-- source tree that produced the test binary exists; when the binary runs
-- elsewhere (e.g. a Nix check derivation, which builds and runs tests in
-- separate sandboxes), the OGMIOS_TEST_PROJECT_ROOT environment variable
-- overrides it at run-time.
getProjectRoot :: Q Exp
getProjectRoot = do
    compileTimeRoot <- makeRelativeToProject ""
    [| Unsafe.unsafePerformIO
        ( fromMaybe $(liftData compileTimeRoot)
            <$> Env.lookupEnv "OGMIOS_TEST_PROJECT_ROOT"
        ) |]
