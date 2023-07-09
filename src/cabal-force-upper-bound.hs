{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Data.Coerce
import Options.Applicative
import Control.Monad
import Data.List
import Text.PrettyPrint hiding ((<>))

import qualified Distribution.Compat.Newtype             as C
import qualified Distribution.Compat.Prelude             as C
import qualified Distribution.FieldGrammar.Newtypes            as C
import qualified Distribution.Fields                          as C
import qualified Distribution.Fields.Field                    as C
import qualified Distribution.Package as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Types.Version as C
import qualified Distribution.Types.VersionInterval as C
import qualified Distribution.Types.VersionRange as C

import Distribution.Pretty (pretty)

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Synthesize constraints to force testing the upper bound of cabal dependencies"
--  <> progDesc "What does this thing do?"
  )
  where
    parser :: Parser (IO ())
    parser = pure work
      <*> switch (long "allow-newer" <> help "also create --allow-newer flags")
      <*> strArgument
          (metavar "CABALFILE" <> help "cabal file to read (.cabal)")

newtype Deps = Deps (M.Map C.PackageName C.Version) deriving Show

instance Semigroup Deps where
    (<>) = coerce (M.unionWith @C.PackageName @C.Version max)
instance Monoid Deps where
    mempty = coerce (M.empty @C.PackageName @C.Version)
    mconcat = coerce (M.unionsWith @[] @C.PackageName @C.Version max)

collectDeps :: BS.ByteString -> Deps
collectDeps contents = deps fields
  where
    fields = case C.readFields contents of
        Left err -> error (show err)
        Right fields' -> fields'

class HasDeps a where deps :: a -> Deps
instance HasDeps a => HasDeps [a] where deps = foldMap deps

type FieldValue a = [C.FieldLine a]
type DependencyField = C.List C.CommaVCat (C.Identity C.Dependency) C.Dependency

instance HasDeps (C.Field a) where
    deps (C.Section _ _ fs) = foldMap deps fs
    deps (C.Field (C.Name _ "build-depends") val) = case C.eitherParsec @DependencyField s of
        Left err -> error $ "Parsing field failed: " ++ show err
        Right depList -> deps depList
      where s = BS.unpack $ BS.unlines $ map C.fieldLineBS val -- BS.unpack for lack of eitherParsecBS
    deps _ = mempty
instance HasDeps c => HasDeps (C.List a b c) where
    deps = deps . C.unpack
instance HasDeps C.Dependency where
    deps (C.Dependency name range _libSet) = goVersionRange name (range)

goVersionRange :: C.PackageName -> C.VersionRange -> Deps
goVersionRange pkgname = foldMap (goVersionInterval pkgname) . C.asVersionIntervals

goVersionInterval :: C.PackageName -> C.VersionInterval -> Deps
goVersionInterval pkgname (C.VersionInterval _lower upper)
    | Just version <- fromUpperBound upper
    = Deps $ M.singleton pkgname version
    -- We could consider the lower bound here, but maybe that is not necessary,
    -- since cabal will take it into account even with --allow-newer
    -- We'll see.
    | otherwise = mempty

-- Given an upper bound, which version (a.b.c.0) do we want to test?
-- (So far only two digit upper bounds are supported, can be extended later)
fromUpperBound :: C.UpperBound -> Maybe C.Version
fromUpperBound C.NoUpperBound = Nothing
fromUpperBound (C.UpperBound (C.versionNumbers -> [a,0]) C.ExclusiveBound) | a > 0 =
    Just $ C.mkVersion [pred a, 0]
fromUpperBound (C.UpperBound (C.versionNumbers -> [a,b]) C.ExclusiveBound) | a > 0 =
    Just $ C.mkVersion [a, pred b]
fromUpperBound _ = Nothing



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


work :: Bool -> FilePath -> IO ()
work allow_newer cabalfile = do
    contents <- BS.readFile cabalfile
    let Deps upper_bounds = collectDeps contents
    putStrLn $ render $ hsep
        -- We are not using C.PackageVersionConstraint's pretty syntax to avoid the space
        [ ("--constraint=" <> pretty pkg <> pretty (C.majorBoundVersion version)) <+>
          (if allow_newer then "--allow-newer=" <> pretty pkg else mempty)
        | (pkg, version) <- M.toList upper_bounds ]
