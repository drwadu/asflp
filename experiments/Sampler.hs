{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sampler (Literal, Interpretation3, Program3, isUnique, atoms, Body, (!!!), randomBounds3, randomProgram3x2) where


import Test.QuickCheck
import Control.Monad (liftM2)
import Test.QuickCheck.Gen (genDouble)
import Data.List (nub)

newtype Literal = Literal Int
  deriving (Eq,Num,Ord,Enum,Show)

tooLarge :: Literal -> a
tooLarge _ = errorWithoutStackTrace "!!: index too large"

{-# INLINABLE (!!!) #-}
(!!!) :: [a] -> Literal -> a
xs !!! l = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) tooLarge xs l


atoms = [1..3]
literals = map Literal $ atoms ++ map negate atoms

instance Arbitrary Literal where
  arbitrary = do
    a <- elements atoms
    l <- elements [a, negate a]
    return . Literal $ l


type Bounds = (Double,Double)

newtype Interpretation3 
  = Interpretation3 [Bounds]
  deriving (Show)

instance Arbitrary Interpretation3 where
  arbitrary = do
    a <- liftM2 (,) genDouble genDouble
    b <- liftM2 (,) genDouble genDouble
    c <- liftM2 (,) genDouble genDouble
    return . Interpretation3 $ [a,b,c]

randomBounds3 = do
    a <- liftM2 (,) genDouble genDouble
    b <- liftM2 (,) genDouble genDouble
    c <- liftM2 (,) genDouble genDouble
    return [a,b,c]


newtype Body 
  = Body [Literal]
  deriving (Eq,Show)

isUnique xs = nub xs == xs
consistent xs = not . any id $ [x + x' == 0 | x <- xs, x' <- xs]
fineBody xs = isUnique xs && consistent xs && (not $ null xs)


instance Arbitrary Body where
  arbitrary = Body <$> sublistOf literals `suchThat` isUnique


newtype Program3 
  = Program3 [[Body]]
  deriving (Eq,Show)

instance Arbitrary Program3 where
  arbitrary = do 
    a <- Body <$> sublistOf (map Literal [2,3,-2,-3]) `suchThat` fineBody 
    a' <- Body <$> sublistOf (map Literal [2,3,-2,-3]) `suchThat` fineBody 
    b <- Body <$> sublistOf (map Literal [1,3,-1,-3]) `suchThat` fineBody 
    b' <- Body <$> sublistOf (map Literal [1,3,-1,-3]) `suchThat` fineBody 
    c <- Body <$> sublistOf (map Literal [1,2,-1,-2]) `suchThat` fineBody
    c' <- Body <$> sublistOf (map Literal [1,2,-1,-2]) `suchThat` fineBody
    return . Program3 $ [[a,a'],[b,b'],[c,c']]

randomProgram3x2 = do
    a <- Body <$> sublistOf (map Literal [2,3,-2,-3]) `suchThat` fineBody 
    a' <- Body <$> sublistOf (map Literal [2,3,-2,-3]) `suchThat` fineBody 
    b <- Body <$> sublistOf (map Literal [1,3,-1,-3]) `suchThat` fineBody 
    b' <- Body <$> sublistOf (map Literal [1,3,-1,-3]) `suchThat` fineBody 
    c <- Body <$> sublistOf (map Literal [1,2,-1,-2]) `suchThat` fineBody
    c' <- Body <$> sublistOf (map Literal [1,2,-1,-2]) `suchThat` fineBody
    return [[a,a'],[b,b'],[c,c']]

--newtype Program3 
--  = Program3 [Body]
--  deriving Show
--
--instance Arbitrary Program3 where
--  arbitrary = do 
--    a <- Body <$> sublistOf (map Literal [2,3,-2,-3]) `suchThat` fineBody 
--    b <- Body <$> sublistOf (map Literal [1,3,-1,-3]) `suchThat` fineBody 
--    c <- Body <$> sublistOf (map Literal [1,2,-1,-2]) `suchThat` fineBody
--    return . Program3 $ [a,b,c]
--
--


