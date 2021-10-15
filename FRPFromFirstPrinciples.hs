module FRPFromFirstPrinciples where

import Data.Maybe

--one :: Behavior Real
--red :: Behavior Color

type Time = Double
time :: Behavior Time
time = \ts -> ts

testTime = at time [0,1,2,3]

{-
lbp :: Event ()
key :: Event Char

color :: Behavior Color
color = red `until` (lbp -=> blue)

ball :: Behavior Piture
ball = paint olor ir

circ :: Behavior Region
circ = translate (cos time, sin time) (cirle 1)

color2 :: Behavior Color
color2 = red `until` (lbp -=> blue) .|. (key -=> yellow)

color3 = red `until` (when (time >* 5) -=> blue)
-}


($*) :: Behavior (a -> b) -> Behavior a -> Behavior b
ff $* fb = \ts -> zipWith ($) (ff ts) (fb ts)

lift0 :: a -> Behavior a
lift0 x = map (const x)

lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 f b1 = lift0 f $* b1

lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 f b1 b2 = lift1 f b1 $* b2


type Behavior a = [Time] -> [a]
type Event a = [Time] -> [Maybe a]

at :: Behavior a -> [Time] -> a
at b ts = last (b ts)

occ :: Event a -> [Time] -> [(Time,a)]
occ e ts = justValuse ts (e ts)
  where
    justValuse :: [Time] -> [Maybe a] -> [(Time, a)]
    justValuse ts maybes = 
      map (\(t,Just a) -> (t,a))
      $ filter (\(_, a) -> isJust a) 
      $ zip ts maybes

{-
instane Num a => Num (Behavior a) where
  (+) = lift2 (+)
-}

---Choice
(.|.) :: Event a -> Event a -> Event a
fe1 .|. fe2 =
  \ts -> zipWith aux (fe1 ts) (fe2 ts)
    where 
      aux Nothing Nothing = Nothing
      aux (Just x) _ = Just x
      aux _ (Just x) = Just x


--- Behavior Switching

until :: Behavior a -> Event (Behavior a) -> Behavior a
fb `until` fe =
  \ts -> loop ts (fe ts) (fb ts)
    where 
      loop ts@(_:ts') ~(e:es) (b:bs) =
        b : case e of
              Nothing -> loop ts' es bs
              Just fb' -> tail (fb' ts)

--- Snapshot

snapshot :: Event a -> Behavior b -> Event (a,b)
snapshot fe fb = 
  \ts -> zipWith aux (fe ts) (fb ts)
    where 
      aux (Just x) y = Just (x,y)
      aux Nothing _ = Nothing


--- Predicate Events

when :: Behavior Bool -> Event ()
when fb =
  \ ts -> let bs = fb ts 
          in zipWith up (True : bs) bs
          where   
            up False True = Just ()
            up _ _ = Nothing


