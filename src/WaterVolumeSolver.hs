{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module WaterVolumeSolver where

import GHC.Generics (Generic)
import System.Environment
import Control.DeepSeq
import Control.Monad.Par
import Control.StopWatch
import Data.Maybe

import qualified Data.ByteString.Char8 as B (readInt,splitWith,readFile,pack,foldl')
import System.Clock (TimeSpec(..),toNanoSecs)
import Data.Sequence (viewl,ViewL(..),viewr,ViewR(..),Seq,
                     (<|),(|>),(><),empty,singleton, splitAt,reverse)
import Data.List(foldl',foldl1')
import qualified Data.Vector as V

data WaterGap = WaterGap {
                    _left     :: Int, --Left highest pillar of this gap
                    _blockSum :: Int, --Total sum of blocks within the gap
                    _blocks   :: Int, --Number of pillars within the gap
                    _right    :: Int  --Right highest pillar of this gap
                } 
                    deriving (Show, Ord, Eq, Generic)

instance NFData WaterGap

type WaterGaps = Seq WaterGap
type PillarLine = (WaterGaps,WaterGaps)

--Some boilerplate code, since Data.Sequence doesn't have these for some reason
sHead :: Seq a -> a
sHead (viewl -> a :< set) = a
sLast :: Seq a -> a
sLast (viewr -> _ :> a) = a
sInit :: Seq a -> Seq a
sInit (viewr -> set :> _) = set
sTail :: Seq a -> Seq a
sTail (viewl -> _ :< set) = set

main = do
    [fpath, n] <- getArgs
    s <- B.readFile fpath
    let list = V.map (fst . fromJust . B.readInt) $ V.fromList $ B.splitWith (==',') s
    --Force the input into memory before computation
    --and stop the timer once the volume has been calculated
    (vol,ts) <- deepseq list $ stopWatch $ do
        let watergaps = runPar $ parEval 300 list
        let v = volume watergaps
        deepseq v $ return v
    putStrLn $ show vol
    putStrLn $ show (fromIntegral (toNanoSecs ts) / 1000000)

parEval :: Int -> V.Vector Int -> Par PillarLine
parEval n xs = do
    mergedWaterGaps <- parEvalChunk n xs
    if length mergedWaterGaps >= n * 2
        then parFoldChunk n mergedWaterGaps
        else return $ foldl' merge (sHead mergedWaterGaps) (sTail mergedWaterGaps) --Final merge

parEvalChunk :: Int -> V.Vector Int -> Par (Seq PillarLine)
parEvalChunk n vec
    | V.null vec = return empty
    | otherwise = do
        let (chunk, rest) = V.splitAt n vec
        nv <- new  --Create a mutable container that will store the result of the computation
        fork $ put nv (solve chunk) --Start parallel computation on a chunk
        ys <- parEvalChunk n rest --Start parallel computation on the rest
        y <- get nv --retrieve the result
        return $ y <| ys

parFoldChunk :: Int -> (Seq PillarLine) -> Par PillarLine
parFoldChunk _ (viewl -> EmptyL) = return (empty,empty)
parFoldChunk n xs = do
    let (chunk, rest) = Data.Sequence.splitAt n xs
    nv <- new
    fork $ put nv (foldl' merge (sHead chunk) (sTail chunk))
    ys <- parFoldChunk n rest
    y <- get nv
    if length rest > 0
        then return $ y `merge` ys
        else return y

maxWGap x = WaterGap maxBound 0 0 x
minWGap x = WaterGap minBound 0 0 x


--Using initial watergaps, compute a sequence of watergaps
--Watergaps can move upward (5,1,1,6), downward (6,1,1,4)
--or remain the same height(5,1,1,5).
--Also, there does not have to be a single upward or downward
--watergap, there can be a sequence of those. Therefore
--the result of this function is two sequences of watergaps
--The first is the sequence of upwards/sameheight watergaps
--and the second is the sequence of downwards watergaps
--This is also explains why I use a maxWaterGap and a minimumWaterGap
--If we only have a single pillar, then I can't decide whether it is
--upward going or downward going, since no watergap could be created
solve :: V.Vector Int -> PillarLine
solve vec 
    | V.null vec = (empty,empty)
    | otherwise = (maxWGap (V.head vec) <| upwGaps, minWGap lim <| dwnGaps)
    where
        (_,_,_,lim,upwGaps) = solveUpw (V.tail vec) (V.head vec)
        (_,_,_,dwnGaps) = solveDownw (V.tail $ V.reverse vec) (V.last vec) lim

solveUpw :: V.Vector Int -> Int -> (Int, Int, Int, Int, WaterGaps)
solveUpw vec mpill = V.foldl' (\(mp,bSum,bNum,lim,wgs) n ->
                        if n >= mp
                            then (n,0,0,n,wgs |> WaterGap mp bSum bNum n)
                            else (mp,bSum + n, bNum + 1, lim, wgs)) (mpill,0,0,mpill,empty) vec

solveDownw :: V.Vector Int -> Int -> Int -> (Int, Int, Int, WaterGaps)
solveDownw vec mpill lim = V.foldl' (\(mp,bSum,bNum, wgs) n ->
                                if n > mp
                                    then (n,0,0, WaterGap n bSum bNum mp <| wgs)
                                    else (mp, bSum + n, bNum + 1, wgs)) (mpill,0,0,empty) vec

--The merge function takes two already solved watergaps and attempts to merge them
--In a sense we are trying to create a single watergap that creates a bridge between
--these the existing sequence of watergaps. We only need to look at the downward watergaps
--of the left PillarLine and the upward watergaps of the right PillarLine.
merge :: PillarLine -> PillarLine -> PillarLine
--In this case only single watergaps exist on both sides. So all we have to do 
--is connect them with a new watergap. We still need to decide, whether the new
--watergap is upward or downward going
merge (us1, viewl -> d:<(viewl -> EmptyL)) (viewl -> u:<(viewl -> EmptyL),ds2)
    | _right d >= _right u = (us1, d <| sWGap <| (sTail ds2))
    | otherwise            = (us1    |> sWGap ,   ds2)
    where sWGap = WaterGap (_right d) 0 0 (_right u)
--First we compare the highest pillars of both pillarlines
--If the pillar of the left pillarline is lower than the other (mergeRight),
--then we need to decide with which pillar this left pillar will create a watergap
--If the highest pillar and the second highest pillar of the right pillarline are both
--higher, then we must make a watergap with the second highest pillar. Since, the second
--highest pillar and highest pillar can contain a watergap, which exceeds the new watergap in height.
--Any watergap below the left pillar and below the second highest pillar will be stripped
--and it's volume added to the new watergap.
merge (us1,ds1) (us2,ds2)
    | _right (sHead ds1) >= _right (sLast us2) = (us1, mergeLeft ds1 upwVol upwMaxPill >< (sTail ds2))
    | otherwise                                = (us1 >< mergeRight us2 (0,0) downVol downMaxPill, ds2)
    where
        upwVol = blVolUp $ sTail us2
        upwMaxPill = _right $ sLast us2
        downVol = blVolDown $ sTail ds1
        downMaxPill = _right $ sHead ds1

mergeLeft,mergeLeftSingle,mergeLeftMany :: WaterGaps -> (Int,Int) -> Int -> WaterGaps
mergeLeft (viewl -> d:<(viewl -> EmptyL)) = mergeLeftSingle (singleton d) --Handle some edge case
mergeLeft ds = mergeLeftMany ds

mergeLeftSingle (viewl -> d:<_) (usSum,usNum) pillar = d <| (singleton $ WaterGap (_right d) usSum usNum pillar)

mergeLeftMany (viewl -> EmptyL) _ _ = empty
mergeLeftMany (viewl -> d :< ds) us@(usSum,usNum) pillar
    --If the downward pillar is higher than the maximum upward pillar
    --then we do not want to create a gap between these two
    | _right d >= pillar && not (null ds) = d <| mergeLeftMany ds us pillar
    | _right d >= pillar                  = mergeLeftSingle (singleton d) us pillar
    | otherwise = --Now the downward pillar is lower, so we use the previous pillar
                  --to create the gap with and calculate the volume of any remaining downward gaps
        let (bSum1, bNum1) = blVolDown (d <| ds)
        in singleton $ WaterGap (_left d) (bSum1 + usSum) (bNum1 + usNum) pillar

mergeRight,mergeRightSingle,mergeRightMany :: WaterGaps -> (Int,Int) -> (Int,Int) -> Int -> WaterGaps
mergeRight (viewl -> u:<(viewl -> EmptyL)) = mergeRightSingle (singleton u) --handle an edge case
mergeRight us = mergeRightMany us

mergeRightSingle (viewl -> u:<_) _ (blSum,blNum) pillar = singleton $ WaterGap pillar blSum blNum (_right u)

mergeRightMany (viewl -> EmptyL) _ _ _ = empty
mergeRightMany (viewl -> u:<us) (blSum,blNum) dsVol@(dsSum,dsNum) pillar
    --For merge right we start from the lowest upward going pillar, until we find a pillar
    --that is equal or higher
    | _right u < pillar = mergeRightMany us (blocksSum + _right u,blocksNum + 1) dsVol pillar
    | otherwise = WaterGap pillar (blocksSum + dsSum) (blocksNum + dsNum) (_right u) <| us
        where 
            blocksSum = blSum + _blockSum u
            blocksNum = blNum + _blocks u

addBlockVolume :: (Int, Int) -> WaterGap -> WaterGap
addBlockVolume (bSum,bNum) (WaterGap l bS bN r) = WaterGap l (bS + bSum) (bN + bNum) r

blVolDown :: WaterGaps -> (Int, Int)
blVolDown = foldl' (\acc wg -> (fst acc + _blockSum wg + _right wg, snd acc + _blocks wg + 1)) (0,0)

blVolUp :: WaterGaps -> (Int, Int)
blVolUp = foldl' (\acc wg -> (fst acc + _blockSum wg + _left wg, snd acc + _blocks wg + 1)) (0,0)

volume :: PillarLine -> Int
volume (lpl,rpl) = f lpl + f rpl
    where
        f = foldl' (\acc wg -> acc + volumeWaterGap wg) 0

--The volume of a watergap [5,2,3,6] is calculated as follows:
--min(5,6) * 2 - (2 + 3) =
--5 * 2 - 5 =
--10 - 5 = 5
volumeWaterGap :: WaterGap -> Int  
volumeWaterGap wg = (min (_left wg) (_right wg)) * (_blocks wg) - (_blockSum wg)