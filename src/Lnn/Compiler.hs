module Lnn.Compiler (compile) where

import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lnn.Logic (Flp, Interpretation, Symbol, bot, top)
import Lnn.Neuro (Lnn (Lnn), Neuron (..), Supports, showw, stringifyAnd, update)
import Lnn.Parser (parse, parseBounds)

compile :: Flp -> Interpretation -> Lnn
compile flp input = Lnn ((Seq.fromList . map (conditionTo input) . toList) nn) n d
  where
    Lnn nn n d = rootify . complete supports atoms . feedNegativeLiterals supports . feedAtoms supports $ emptyLnn
    emptyLnn = Lnn Seq.empty 0 (-1.0)
    atoms = Map.keys supports
    supports = atomsSupports flp
    conditionTo m atom@(Atom s _ _) = maybe atom (uncurry (update atom)) $ Map.lookup s m
    conditionTo _ x = x

-- | constructs completion as LNN
complete :: Supports -> [String] -> Lnn -> Lnn
complete _ [] lnn = lnn
complete supports (x : xs) lnn = complete supports xs (proof supports lnn x)

-- | constructs the formula that serves as a proof for a given atom
proof :: Supports -> Lnn -> String -> Lnn
proof supports lnn@(Lnn nn n d) atom = if rhs /= [] then Lnn (nn Seq.>< Seq.fromList neuronsToAdd) (n + length neuronsToAdd) d else lnn
  where
    neuronsToAdd
        | nSupports > 1 =
            constructedSupports
                ++ [ Or idxs bot top
                   , IfThen bodyFormulaIdx atomIdx bot top
                   , IfThen atomIdx bodyFormulaIdx bot top
                   , Proof (bodyFormulaIdx + 1) (bodyFormulaIdx + 2) bot top
                   ]
        | not (null constructedSupports) =
            constructedSupports
                ++ [ IfThen (k + 1) atomIdx bot top
                   , IfThen atomIdx (k + 1) bot top
                   , Proof (k + 2) (k + 3) bot top
                   ]
        | otherwise =
            [ IfThen (head idxs) atomIdx bot top
            , IfThen atomIdx (head idxs) bot top
            , Proof (k + 1) (k + 2) bot top
            ]
    atomIdx = findNeuronIndex lnn atom
    bodyFormulaIdx = k + length constructedSupports + 1
    nSupports = length idxs
    idxs = givenSupportsIdxs ++ [i + k | i <- [1 .. (length constructedSupports)]]
    (givenSupportsIdxs, constructedSupports) = partitionEithers . map (andify lnn) $ rhs
    rhs = fromMaybe [] $ Map.lookup atom supports
    k = n - 1

-- | finds index of neuron within ast
findNeuronIndex :: Lnn -> String -> Int
findNeuronIndex lnn@(Lnn nn _ _) s = fromMaybe (-1) $ Seq.findIndexL ((==) s . showw lnn) nn

-- | finds neuron within ast
findNeuron :: Lnn -> String -> Maybe Neuron
findNeuron lnn@(Lnn nn _ _) s = Seq.index nn <$> Seq.findIndexL ((==) s . showw lnn) nn

{- | returns Left of index within lnn of neuron that represents
conjunction over xs if respective neuron exists in lnn.
otherwise constructs this very neuron and returns Right of
this neuron
-}
andify :: Lnn -> [String] -> Either Int Neuron
andify lnn xs =
    if length xs > 1
        then case findNeuronIndex lnn x of
            -1 -> Right $ And (map (findNeuronIndex lnn) xs') bot top
            n -> Left n
        else Left . findNeuronIndex lnn . head $ xs
  where
    x = stringifyAnd xs'
    xs' = sort xs

atomsSupports :: Symbol -> Supports
atomsSupports = parse Map.empty

feedAtoms :: Supports -> Lnn -> Lnn
feedAtoms supports (Lnn nn n d) = Lnn (nn Seq.>< lits) (n + length lits) d
  where
    lits = Seq.fromList . Set.toList . Set.fromList $ map litify $ atoms ++ supportsBodiesAtoms
    litify lit = if head lit == '-' then Atom (tail lit) bot top else Atom lit bot top
    atoms = Map.keys supports
    supportsBodiesAtoms = concat . concat . Map.foldr (:) [] $ supports

feedNegativeLiterals :: Supports -> Lnn -> Lnn
feedNegativeLiterals supports lnn@(Lnn nn n d) = Lnn (nn Seq.>< lits) (n + length lits) d
  where
    lits = Seq.fromList . Set.toList . Set.fromList . map notify $ supportsBodies
    notify lit = Not (findNeuronIndex lnn lit) bot top
    supportsBodies = Set.toList . Set.fromList . map tail . filter ((==) '-' . head) $ concat . concat . Map.foldr (:) [] $ supports

rootify lnn@(Lnn nn n d) = Lnn (nn Seq.|> And (idxs nn) bot top) (n + 1) d
  where
    idxs = map (findNeuronIndex lnn . showw lnn) . toList . Seq.filter isProof
    isProof (Proof{}) = True
    isProof _ = False
