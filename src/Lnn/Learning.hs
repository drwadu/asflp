{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE LambdaCase                               #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE PartialTypeSignatures                    #-}
{-# LANGUAGE PatternSynonyms                          #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeInType                               #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}
--{-# OPTIONS_GHC -fno-warn-orphans                     #-}
--{-# OPTIONS_GHC -fno-warn-partial-type-signatures     #-}
--{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
--{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
--{-# OPTIONS_GHC -fwarn-redundant-constraints          #-}

module Lnn.Learning 
  (myFunc, (:&))
where

import           Numeric.Backprop                      ( Backprop, BVar, Reifies, pattern T2, W
                                                       , (^^.), auto, evalBP, evalBP2, gradBP, isoVar2, sequenceVar )
import           GHC.Generics                          ( Generic )
import           GHC.TypeNats                          ( KnownNat, type (<=), type (+) )
import           System.Random                         ( Random(random,randomR), randomIO )
import           Lens.Micro                            ( Lens )
import           Data.List                             ( foldl', unfoldr )


--myFunc x = sqrt (x * 4)
myFunc x y = x + y

data a :& b = !a :& !b
  deriving (Show, Generic)
infixr 2 :&

type Model p a b = forall z. Reifies z W
                => BVar z p
                -> BVar z a
                -> BVar z b

--pattern (:&&) :: (Backprop a, Backprop b, Reifies z W)
--              => BVar z a -> BVar z b -> BVar z (a :& b)
--pattern x :&& y <- (\xy -> (xy ^^. t1, xy ^^. t2)->(x, y))
--  where
--    (:&&) = isoVar2 (:&) (\case x :& y -> (x, y))
--{-# COMPLETE (:&&) #-}
--
--t1 :: Lens (a :& b) (a' :& b) a a'
--t1 f (x :& y) = (:& y) <$> f x
--
--t2 :: Lens (a :& b) (a :& b') b b'
--t2 f (x :& y) = (x :&) <$> f y
--
--linReg :: Model (Double :& Double) Double Double
--linReg (a :& b) x = b * x + a
--
--squaredErrorGrad
--    :: (Backprop p, Backprop b, Num b)
--    => Model p a b      -- ^ Model
--    -> a                -- ^ Observed input
--    -> b                -- ^ Observed output
--    -> p                -- ^ Parameter guess
--    -> p                -- ^ Gradient
--squaredErrorGrad f x targ = gradBP $ \p ->
--    (f p (auto x) - auto targ) ^ 2
--
--trainModel
--    :: (Fractional p, Backprop p, Num b, Backprop b)
--    => Model p a b      -- ^ model to train
--    -> p                -- ^ initial parameter guess
--    -> [(a,b)]          -- ^ list of observations
--    -> p                -- ^ updated parameter guess
--trainModel f = foldl' $ \p (x,y) -> p - 0.1 * squaredErrorGrad f x y p
--
--trainModelIO
--    :: (Fractional p, Backprop p, Num b, Backprop b, Random p)
--    => Model p a b      -- ^ model to train
--    -> [(a,b)]          -- ^ list of observations
--    -> IO p             -- ^ parameter guess
--trainModelIO m xs = do
--    p0 <- (/ 10) . subtract 0.5 <$> randomIO
--    return $ trainModel m p0 xs
