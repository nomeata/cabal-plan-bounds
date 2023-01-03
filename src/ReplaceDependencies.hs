{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ReplaceDependencies (replaceDependencies) where

import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

import qualified Distribution.Fields                          as C
import qualified Distribution.Fields.Field                    as C
import qualified Distribution.FieldGrammar.Newtypes            as C
import qualified Distribution.Compat.Prelude             as C
import qualified Distribution.Compat.Newtype             as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Parsec.Position             as C
import qualified Distribution.Types.Dependency as C
import qualified Distribution.Types.PackageName as C
import qualified Distribution.Types.VersionRange as C

-- | This is the simple and clean entry point to this ugly and hacky module.
-- I hope by the time this tool becomes practically relevant I can replace this module
-- with something cleaner (e.g. based on the Cabal exact-print work)
-- Contributions are highly welcome!
--
-- TODO: Error handling
replaceDependencies ::
    (C.PackageName -> C.VersionRange -> C.VersionRange) -> BS.ByteString -> BS.ByteString
replaceDependencies f contents = changed
  where
    fields = case C.readFields contents of
        Left err -> error (show err)
        Right fields' -> fields'

    buildDeps = findBuildDeps fields
    changed = replaceFieldValues
        [ (fv, BS.pack $ C.prettyShow @DependencyField $ C.pack deps')
        | fv <- buildDeps
        , let deps = parseFieldValue fv
        , let deps' = map (\(C.Dependency name range libSet) -> C.Dependency name (f name range) libSet) deps
        ] contents

type FieldValue a = [C.FieldLine a]
type DependencyField = C.List C.CommaVCat (C.Identity C.Dependency) C.Dependency

findBuildDeps :: [C.Field a] -> [FieldValue a]
findBuildDeps = concatMap go
  where
    go (C.Field (C.Name _ "build-depends") []) = [] -- ignore empty build-depends
    go (C.Field (C.Name _ "build-depends") val) = [val]
    go (C.Field _ _) = []
    go (C.Section _ _ fs) = concatMap go fs

parseFieldValue :: FieldValue a -> [C.Dependency]
parseFieldValue fv =
    case C.eitherParsec @DependencyField s of
        Left err -> error $ "Parsing field failed: " ++ show err
        Right depList -> C.unpack depList
  where
    s = BS.unpack $ BS.unlines $ map C.fieldLineBS fv -- BS.unpack for lack of eitherParsecBS


replaceFieldValues :: [(FieldValue C.Position, BS.ByteString)] -> BS.ByteString -> BS.ByteString
replaceFieldValues valueSubsts input =
    BS.unlines $ mapMaybe substLine $ byLine lineSubsts (BS.lines input)
  where
    lineSubsts :: [(C.FieldLine C.Position, BS.ByteString)]
    lineSubsts =
        sortOn (C.fieldLineAnn . fst) $ concat
        [ (l, r) : [ (l', "") | l' <- ls ] | (l:ls, r) <- valueSubsts ]

    byLine :: [(C.FieldLine C.Position, a)] -> [BS.ByteString] ->
          [ ([(C.FieldLine C.Position, a)], BS.ByteString) ]
    byLine = go 1
      where
        go _ [] ls = unchanged ls
        go _ _ [] = error "Left over field values"
        go i ss ls@(l:ls')
            | r > i = let (ls1, ls2) = splitAt (r - i) ls
                      in unchanged ls1 ++ go (i + length ls1) ss ls2
            | r == i = let (ss1, ss2) = span isHere ss
                      in (ss1, l) : go (i+1) ss2 ls'
            | otherwise = error $ "Field value out of order"
          where
            isHere (fl, _) = C.positionRow (C.fieldLineAnn fl) == i
            r = C.positionRow (C.fieldLineAnn (fst (head ss)))


        unchanged ls =  [ ([], l) | l <- ls ]

    substLine ([], line) = Just line
    substLine (substs, line) =
        let l' = go 1 substs line in
        if BS.all C.isSpace l' then Nothing else Just l'
      where
        go :: Int -> [(C.FieldLine C.Position, BS.ByteString)] -> BS.ByteString -> BS.ByteString
        go _ [] l = l
        go _ _ "" = error "Left over subst"
        go i ((fl,r):ss) l = case BS.stripPrefix old l' of
            Just l2 -> l1 <> r' <> go (i + BS.length l1 + BS.length old) ss l2
            Nothing -> error $ "Did not find expected field value " ++ show old
          where
            c = C.positionCol (C.fieldLineAnn fl)
            (l1, l') = BS.splitAt (c - i) l
            old = C.fieldLineBS fl
            rlines = BS.lines r
            r' = BS.intercalate ("\n" <> BS.replicate (c - 1) ' ') rlines

