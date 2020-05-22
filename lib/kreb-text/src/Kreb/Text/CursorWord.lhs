> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Text.CursorWord where

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, braceList, (<+>))
> import           Kreb.Prop

> data CursorWord
>   = E   | X
>   | P   | R   | L   | W
>   | PP  | PL  | RP  | RL
>   | RW  | PW  | WL  | WP
>   | RWL | PWL | RWP | PWP
>   deriving (Eq, Show)

> instance Fmt.Display CursorWord where
>   display x = case x of
>     E   -> "E"
>     X   -> "X"
>     P   -> "P"
>     R   -> "R"
>     L   -> "L"
>     W   -> "W"
>     PP  -> "PP"
>     PL  -> "PL"
>     RP  -> "RP"
>     RL  -> "RL"
>     RW  -> "RW"
>     PW  -> "PW"
>     WL  -> "WL"
>     WP  -> "WP"
>     RWL -> "RWL"
>     PWL -> "PWL"
>     RWP -> "RWP"
>     PWP -> "PWP"

> isRegionBoundary
>   :: CursorWord -> CursorWord -> Bool
> isRegionBoundary x y =
>   (isSnocP x && isConsR y) || (isSnocL x && isConsP y)
>   where
>     isSnocP u = case u of
>       P -> True; PP -> True; RP -> True; WP -> True; RWP -> True; PWP -> True; _ -> False
>     isConsR u = case u of
>       R -> True; RP -> True; RL -> True; RW -> True; RWL -> True; RWP -> True; _ -> False
>     isSnocL u = case u of
>       L -> True; PL -> True; RL -> True; WL -> True; RWL -> True; PWL -> True; _ -> False
>     isConsP u = case u of
>       P -> True; PP -> True; PL -> True; PW -> True; PWL -> True; PWP -> True; _ -> False

> instance Arb CursorWord where
>   arb = oneFrom
>     [ E,  X,  P,  R,  L,  W,   PP,  PL,  RP
>     , RL, RW, PW, WL, WP, RWL, PWL, RWP, PWP
>     ]
> 
> instance Prune CursorWord where
>   prune _ = []

> instance Semigroup CursorWord where
>   u <> v = case (u,v) of
>     (E,_) -> v;  (_,E) -> u
>     (X,_) -> X;  (_,X) -> X

>     -- P <> v
>     (P,P)   -> PP;  (P,R)   -> W;   (P,L)   -> PL;  (P,W)   -> PW
>     (P,PP)  -> PWP; (P,PL)  -> PWL; (P,RP)  -> WP;  (P,RL)  -> WL
>     (P,RW)  -> W;   (P,PW)  -> PW;  (P,WL)  -> PWL; (P,WP)  -> PWP
>     (P,RWL) -> WL;  (P,PWL) -> PWL; (P,RWP) -> WP;  (P,PWP) -> PWP

>     -- R <> v
>     (R,P)   -> RP;  (R,R)   -> X;   (R,L)   -> RL;  (R,W)   -> RW
>     (R,PP)  -> RWP; (R,PL)  -> RWL; (R,RP)  -> X;   (R,RL)  -> X
>     (R,RW)  -> X;   (R,PW)  -> RW;  (R,WL)  -> RWL; (R,WP)  -> RWP
>     (R,RWL) -> X;   (R,PWL) -> RWL; (R,RWP) -> X;   (R,PWP) -> RWP

>     -- L <> v
>     (L,P)   -> W;   (L,R)   -> X;   (L,L)   -> X;   (L,W)   -> X
>     (L,PP)  -> WP;  (L,PL)  -> WL;  (L,RP)  -> X;   (L,RL)  -> X
>     (L,RW)  -> X;   (L,PW)  -> W;   (L,WL)  -> X;   (L,WP)  -> X
>     (L,RWL) -> X;   (L,PWL) -> WL;  (L,RWP) -> X;   (L,PWP) -> WP

>     -- W <> v
>     (W,P)   -> WP;  (W,R)   -> X;   (W,L)   -> WL;  (W,W)   -> W
>     (W,PP)  -> WP;  (W,PL)  -> WL;  (W,RP)  -> X;   (W,RL)  -> X
>     (W,RW)  -> X;   (W,PW)  -> W;   (W,WL)  -> WL;  (W,WP)  -> WP
>     (W,RWL) -> X;   (W,PWL) -> WL;  (W,RWP) -> X;   (W,PWP) -> WP

>     -- PP <> v
>     (PP,P)   -> PWP; (PP,R)   -> PW;  (PP,L)   -> PWL; (PP,W)   -> PW
>     (PP,PP)  -> PWP; (PP,PL)  -> PWL; (PP,RP)  -> PWP; (PP,RL)  -> PWL
>     (PP,RW)  -> PW;  (PP,PW)  -> PW;  (PP,WL)  -> PWL; (PP,WP)  -> PWP
>     (PP,RWL) -> PWL; (PP,PWL) -> PWL; (PP,RWP) -> PWP; (PP,PWP) -> PWP

>     -- PL <> v
>     (PL,P)   -> PW;  (PL,R)   -> X;   (PL,L)   -> X;   (PL,W)   -> X
>     (PL,PP)  -> PWP; (PL,PL)  -> PWL; (PL,RP)  -> X;   (PL,RL)  -> X
>     (PL,RW)  -> X;   (PL,PW)  -> PW;  (PL,WL)  -> X;   (PL,WP)  -> X
>     (PL,RWL) -> X;   (PL,PWL) -> PWL; (PL,RWP) -> X;   (PL,PWP) -> PWP

>     -- RP <> v
>     (RP,P)   -> RWP; (RP,R)   -> RW;  (RP,L)   -> RWL; (RP,W)   -> RW
>     (RP,PP)  -> RWP; (RP,PL)  -> RWL; (RP,RP)  -> RWP; (RP,RL)  -> RWL
>     (RP,RW)  -> RW;  (RP,PW)  -> RW;  (RP,WL)  -> RWL; (RP,WP)  -> RWP
>     (RP,RWL) -> RWL; (RP,PWL) -> RWL; (RP,RWP) -> RWP; (RP,PWP) -> RWP

>     -- RL <> v
>     (RL,P)   -> RW;  (RL,R)   -> X;   (RL,L)   -> X;   (RL,W)   -> X
>     (RL,PP)  -> RWP; (RL,PL)  -> RWL; (RL,RP)  -> X;   (RL,RL)  -> X
>     (RL,RW)  -> X;   (RL,PW)  -> RW;  (RL,WL)  -> X;   (RL,WP)  -> X
>     (RL,RWL) -> X;   (RL,PWL) -> RWL; (RL,RWP) -> X;   (RL,PWP) -> RWP

>     -- RW <> v
>     (RW,P)   -> RWP; (RW,R)   -> X;   (RW,L)   -> RWL; (RW,W)   -> RW
>     (RW,PP)  -> RWP; (RW,PL)  -> RWL; (RW,RP)  -> X;   (RW,RL)  -> X
>     (RW,RW)  -> X;   (RW,PW)  -> RW;  (RW,WL)  -> RWL; (RW,WP)  -> RWP
>     (RW,RWL) -> X;   (RW,PWL) -> RWL; (RW,RWP) -> X;   (RW,PWP) -> RWP

>     -- PW <> v
>     (PW,P)   -> PWP; (PW,R)   -> X;   (PW,L)   -> PWL; (PW,W)   -> PW
>     (PW,PP)  -> PWP; (PW,PL)  -> PWL; (PW,RP)  -> X;   (PW,RL)  -> X
>     (PW,RW)  -> X;   (PW,PW)  -> PW;  (PW,WL)  -> PWL; (PW,WP)  -> PWP
>     (PW,RWL) -> X;   (PW,PWL) -> PWL; (PW,RWP) -> X;   (PW,PWP) -> PWP

>     -- WL <> v
>     (WL,P)   -> W;   (WL,R)   -> X;   (WL,L)   -> X;   (WL,W)   -> X
>     (WL,PP)  -> WP;  (WL,PL)  -> WL;  (WL,RP)  -> X;   (WL,RL)  -> X
>     (WL,RW)  -> X;   (WL,PW)  -> W;   (WL,WL)  -> X;   (WL,WP)  -> X
>     (WL,RWL) -> X;   (WL,PWL) -> WL;  (WL,RWP) -> X;   (WL,PWP) -> WP

>     -- WP <> v
>     (WP,P)   -> WP;  (WP,R)   -> W;   (WP,L)   -> WL;  (WP,W)   -> W
>     (WP,PP)  -> WP;  (WP,PL)  -> WL;  (WP,RP)  -> WP;  (WP,RL)  -> WL
>     (WP,RW)  -> W;   (WP,PW)  -> W;   (WP,WL)  -> WL;  (WP,WP)  -> WP
>     (WP,RWL) -> WL;  (WP,PWL) -> WL;  (WP,RWP) -> WP;  (WP,PWP) -> WP

>     -- RWL <> v
>     (RWL,P)   -> RW;  (RWL,R)   -> X;   (RWL,L)   -> X;   (RWL,W)   -> X
>     (RWL,PP)  -> RWP; (RWL,PL)  -> RWL; (RWL,RP)  -> X;   (RWL,RL)  -> X
>     (RWL,RW)  -> X;   (RWL,PW)  -> RW;  (RWL,WL)  -> X;   (RWL,WP)  -> X
>     (RWL,RWL) -> X;   (RWL,PWL) -> RWL; (RWL,RWP) -> X;   (RWL,PWP) -> RWP

>     -- PWL <> v
>     (PWL,P)   -> PW;  (PWL,R)   -> X;   (PWL,L)   -> X;   (PWL,W)   -> X
>     (PWL,PP)  -> PWP; (PWL,PL)  -> PWL; (PWL,RP)  -> X;   (PWL,RL)  -> X
>     (PWL,RW)  -> X;   (PWL,PW)  -> PW;  (PWL,WL)  -> X;   (PWL,WP)  -> X
>     (PWL,RWL) -> X;   (PWL,PWL) -> PWL; (PWL,RWP) -> X;   (PWL,PWP) -> PWP

>     -- RWP <> v
>     (RWP,P)   -> RWP; (RWP,R)   -> RW;  (RWP,L)   -> RWL; (RWP,W)   -> RW
>     (RWP,PP)  -> RWP; (RWP,PL)  -> RWL; (RWP,RP)  -> RWP; (RWP,RL)  -> RWL
>     (RWP,RW)  -> RW;  (RWP,PW)  -> RW;  (RWP,WL)  -> RWL; (RWP,WP)  -> RWP
>     (RWP,RWL) -> RWL; (RWP,PWL) -> RWL; (RWP,RWP) -> RWP; (RWP,PWP) -> RWP

>     -- PWP <> v
>     (PWP,P)   -> PWP; (PWP,R)   -> PW;  (PWP,L)   -> PWL; (PWP,W)   -> PW
>     (PWP,PP)  -> PWP; (PWP,PL)  -> PWL; (PWP,RP)  -> PWP; (PWP,RL)  -> PWL
>     (PWP,RW)  -> PW;  (PWP,PW)  -> PW;  (PWP,WL)  -> PWL; (PWP,WP)  -> PWP
>     (PWP,RWL) -> PWL; (PWP,PWL) -> PWL; (PWP,RWP) -> PWP; (PWP,PWP) -> PWP

> instance Monoid CursorWord where
>   mempty = E

> data CursorData = CursorData
>   { hasFocus   :: Bool
>   , pointCount :: Int
>   , markCount  :: Int
>   , cursorWord :: CursorWord
>   } deriving (Eq, Show)

> instance Fmt.Display CursorData where
>   display x = "CursorData" <+> braceList
>     [ display (hasFocus x)
>     , display (pointCount x)
>     , display (markCount x)
>     , display (cursorWord x)
>     ]
> 
> instance Semigroup CursorData where
>   u <> v = CursorData
>     { hasFocus   = (hasFocus u) || (hasFocus v)
>     , pointCount = (pointCount u) + (pointCount v)
>     , markCount  = (markCount u) + (markCount v)
>     , cursorWord = (cursorWord u) <> (cursorWord v)
>     }
> 
> instance Monoid CursorData where
>   mempty = CursorData False 0 0 mempty
> 
> instance Arb CursorData where
>   arb = CursorData <$> arb <*> arb <*> arb <*> arb
> 
> instance Prune CursorData where
>   prune _ = []
