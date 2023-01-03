{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Options.Applicative
import Control.Monad (join)
import Data.Maybe
import Cabal.Plan

import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Package as C
import qualified Distribution.Types.Version as C
import qualified Distribution.Types.VersionRange as C

import ReplaceDependencies


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "General program title/description"
  <> progDesc "What does this thing do?"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> many (strOption
            (  long "plan"
            <> short 'i'
            <> metavar "FILE"
            <> help "plan file to read (.json)"
            ))
        <*> strOption
            (  long "cabal"
            <> short 'c'
            <> metavar "FILE"
            <> help "cabal file to edit (.cabal)"
            )

cabalPackageName :: BS.ByteString -> C.PackageName
cabalPackageName contents =
    case C.runParseResult (C.parseGenericPackageDescription contents) of
        (_warn, Left err) -> error (show err)
        (_warn, Right gpd) -> C.packageName gpd

depsOf :: C.PackageName -> PlanJson -> M.Map C.PackageName C.Version
depsOf pname plan = M.fromList -- TODO: What if different units of the package have different deps?
 [ (C.mkPackageName (T.unpack depName), C.mkVersion depVersion)
 | unit <- M.elems (pjUnits plan)
 , let PkgId (PkgName pname') _ = uPId unit
 , pname' == T.pack (C.unPackageName pname)
 , comp <- M.elems (uComps unit)
 , depUid <- S.toList (ciLibDeps comp)
 , let depunit = pjUnits plan M.! depUid
 , let PkgId (PkgName depName) (Ver depVersion) = uPId depunit
 ]


work :: [FilePath] -> FilePath -> IO ()
work planfiles cabalfile = do
    contents <- BS.readFile cabalfile

    -- Figure out package name
    let pname = cabalPackageName contents

    plans <- mapM decodePlanJson planfiles

    let deps = M.unionsWith C.unionVersionRanges $ map (fmap C.majorBoundVersion) $ map (depsOf pname) plans


    BS.putStrLn $ replaceDependencies (\pn vr -> fromMaybe vr $ M.lookup pn deps) contents
