{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Options.Applicative
import Control.Monad
import Data.List
import Cabal.Plan
import Text.PrettyPrint hiding ((<>))

import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Package as C
import qualified Distribution.Types.Version as C
import qualified Distribution.Types.VersionRange as C
import Distribution.Pretty (pretty)

import ReplaceDependencies


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Derives dependency bounds from build plans"
--  <> progDesc "What does this thing do?"
  )
  where
    parser :: Parser (IO ())
    parser = pure work
      <*> switch (long "dry-run" <> short 'n' <> help "do not actually write .cabal files")
      <*> switch (long "extend" <> help "only extend version ranges")
      <*> many (argument
          (is ".json")
          (metavar "PLAN" <> help "plan file to read (.json)"))
      <*> many (strOption
          (short 'c' <> long "cabal" <>
           metavar "CABALFILE" <> help "cabal file to update (.cabal)"))

    is :: String -> ReadM FilePath
    is suffix = maybeReader $ \s -> do
        guard (suffix `isSuffixOf` s)
        pure s

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

unionMajorBounds1 :: [C.Version] -> C.VersionRange
unionMajorBounds1 [] = C.anyVersion
unionMajorBounds1 vs = foldr1 C.unionVersionRanges (map C.majorBoundVersion vs)

unionMajorBounds :: C.VersionRange -> [C.Version] -> C.VersionRange
unionMajorBounds vr [] = vr
unionMajorBounds vr vs = C.unionVersionRanges vr (unionMajorBounds1 vs)

-- assumes sorted input
pruneVersionRanges :: [C.Version] -> [C.Version]
pruneVersionRanges [] = []
pruneVersionRanges [v] = [v]
pruneVersionRanges (v1:v2:vs)
  | v2 `C.withinRange` C.majorBoundVersion v1 = pruneVersionRanges (v1 : vs)
  | otherwise                                 = v1 : pruneVersionRanges (v2 : vs)


-- Assumes that the “new” range is always the same
cleanChanges :: [(C.PackageName, C.VersionRange, C.VersionRange)]
    -> [(C.PackageName, ([C.VersionRange], C.VersionRange))]
cleanChanges changes =
    M.toList $
    M.map (\(old, new) -> (nub old, new)) $ -- No Ord C.VersionRange
    M.fromListWith (\(olds1, new1) (olds2, _new2) -> (olds1 <> olds2, new1)) $
    [ (pname, ([old], new)) | (pname, old, new) <- changes, old /= new ]


work :: Bool -> Bool -> [FilePath] -> [FilePath] -> IO ()
work dry_run extend planfiles cabalfiles = do
    plans <- mapM decodePlanJson planfiles

    forM_ cabalfiles $ \cabalfile -> do
      contents <- BS.readFile cabalfile

      -- Figure out package name
      let pname = cabalPackageName contents

      let deps = fmap (pruneVersionRanges . sort) $
              M.unionsWith (++) $
              map (fmap pure) $
              map (depsOf pname) plans

      let new_deps pn vr
            | pn == pname = C.anyVersion -- self-dependency
            | Just vs' <- M.lookup pn deps
            = if extend
              then unionMajorBounds vr [ v | v <- vs' , not (v `C.withinRange` vr) ]
              else unionMajorBounds1 vs'
            | otherwise  = vr -- fallback

      let (contents', fieldChanges) = replaceDependencies new_deps contents

      forM_ (cleanChanges fieldChanges) $ \(pn, (olds, new)) ->
        putStrLn $ render $
            hang (pretty pn) 4 $ vcat $
                [ char '-' <+> pretty old | old <- olds ] <>
                [ char '+' <+> pretty new ]

      unless dry_run $
          unless (contents == contents') $
              -- TODO: Use atomic-write
              BS.writeFile cabalfile contents'
