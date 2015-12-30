{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Breaking algorithm used to split a paragraph into lines
-- or a text into pages
---------------------------------------------------------
-- #hide
module Graphics.PDF.Typesetting.Breaking (
   Letter(..)
 , formatList
 , infinity
 , createChar
 , kernBox
 , glueBox
 , penalty
 , spaceGlueBox
 , hyphenPenalty
 , splitText
 , MaybeGlue(..)
 , defaultBreakingSettings
 , BRState(..)
 , glueSize
 , mkLetter
 , spaceWidth
 , centeredDilatationFactor
 , leftDilatationFactor
 , rightDilatationFactor
 , dilatationRatio
 , badness
 , bigAdjustRatio
 , Justification(..)
 , simplify
 ) where
     
import Graphics.PDF.LowLevel.Types
import Data.List(minimumBy)  
import qualified Data.Map as Map
import Graphics.PDF.Text
import Graphics.PDF.Typesetting.Box
import Data.Maybe(fromJust)
import Graphics.PDF.Hyphenate

--import Debug.Trace

data Justification = FullJustification
                   | Centered
                   | LeftJustification
                   | RightJustification
                   deriving(Eq)

-- | Make a letter from any box
mkLetter :: (Show a, Box a, DisplayableBox a) => BoxDimension -- ^ Dimension of the box
         -> Maybe s -- ^ Text style of the box (can use t)
         -> a  -- ^ Box
         -> Letter s
mkLetter d s a = Letter d (AnyBox a) s

-- | A letter which can be anything. Sizes are widths and for glue the dilation and compression factors
-- For the generic letter, height and descent are also provided
data Letter s = Letter BoxDimension !AnyBox !(Maybe s) -- ^ Any box as a letter
              | Glue !PDFFloat !PDFFloat !PDFFloat !(Maybe s) -- ^ A glue with style to know if it is part of the same sentence
              | FlaggedPenalty !PDFFloat !Int !s -- ^ Hyphen point
              | Penalty !Int -- ^ Penalty
              | AChar !s !Char !PDFFloat -- ^ A char
              | Kern !PDFFloat !(Maybe s) -- ^ A kern : non dilatable and non breakable glue
             
class MaybeGlue a where
    glueY :: a -> PDFFloat
    glueZ :: a -> PDFFloat
    glueSizeWithRatio :: a -> PDFFloat -> PDFFloat
    
instance MaybeGlue (Letter s) where
    glueSizeWithRatio = letterWidth
    glueY (Glue _ y _ _) = y
    glueY _ = 0
    glueZ (Glue _ _ z _) = z
    glueZ _ = 0
    
           
-- | Compute glue width with dilation
glueSize :: PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat
glueSize w y z r =   
          if r >= 0 
              then
                  r*y + w
              else
                  r*z + w
                  
letterWidth :: Letter s -- ^ letter
            -> PDFFloat -- ^ Adjustement ratio
            -> PDFFloat -- ^ Width
letterWidth (AChar _ _ w) _ = w
letterWidth (Letter dim _ _) _ = boxWidth dim
letterWidth (Glue w yi zi _) r =  glueSize w yi zi r
letterWidth (FlaggedPenalty _ _ _) _ = 0
letterWidth (Penalty _) _ = 0
letterWidth (Kern w _) _ = w
             
instance Show (Letter s) where
   show (Letter _ a _) = "(Letter " ++ show a ++ ")"
   show (Glue a b c _) = "(Glue " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"
   show (FlaggedPenalty a b _) = "(FlaggedPenalty " ++ show a ++ " " ++ show b ++  ")"
   show (Penalty a) = "(Penalty " ++ show a ++ ")"
   show (AChar _ t _) = "(Text " ++ show t ++ ")"
   show (Kern _ _) = "(Kern)"

type CB a = (PDFFloat,PDFFloat,PDFFloat,Int,a)


class PointedBox s a | a -> s where
    isFlagged :: a -> Bool
    getPenalty :: a -> Int
    isPenalty :: a -> Bool
    letter :: a -> Letter s
    position :: a -> Int
    cumulatedW :: a -> PDFFloat
    cumulatedY :: a -> PDFFloat
    cumulatedZ :: a -> PDFFloat
    isForcedBreak :: a -> Bool
    
instance PointedBox s (PDFFloat,PDFFloat,PDFFloat,Int,Letter s)  where
    isFlagged (_,_,_,_,FlaggedPenalty _ _ _) = True    
    isFlagged _ = False
    isPenalty (_,_,_,_,FlaggedPenalty _ _ _) = True    
    isPenalty (_,_,_,_,Penalty _) = True    
    isPenalty _ = False
    getPenalty (_,_,_,_,FlaggedPenalty _ p _) = p
    getPenalty (_,_,_,_,Penalty p) = p
    getPenalty _ = 0
    letter (_,_,_,_,a) = a
    position (_,_,_,p,_) = p
    cumulatedW (w,_,_,_,_) = w
    cumulatedY (_,y,_,_,_) = y
    cumulatedZ (_,_,z,_,_) = z
    isForcedBreak (_,_,_,_,FlaggedPenalty _ p _) = p <= (-infinity)
    isForcedBreak (_,_,_,_,Penalty p) = p <= (-infinity)
    isForcedBreak _ = False
    
    
instance PointedBox s (ZList s) where
    isPenalty (ZList _ b _) = isPenalty b
    isFlagged (ZList _ b _) = isFlagged b
    letter (ZList _ b _) = letter b
    position (ZList _ b _) = position b
    cumulatedW (ZList _ b _) = cumulatedW b
    cumulatedY (ZList _ b _) = cumulatedY b
    cumulatedZ (ZList _ b _) = cumulatedZ b
    getPenalty (ZList _ b _) = getPenalty b
    isForcedBreak (ZList _ b _) = isForcedBreak b
    
-- A penalty has no width unless you break on it so it needs a special processing
penaltyWidth :: Letter s -> PDFFloat
penaltyWidth (FlaggedPenalty w _ _) = w
penaltyWidth _ = 0

data BreakNode  = 
                 BreakNode  { totalWidth :: !PDFFloat
                            , totalDilatation :: !PDFFloat
                            , totalCompression :: !PDFFloat
                            , demerit :: !PDFFloat
                            , flagged :: !Bool
                            , fitnessValue :: !Int
                            , ratio :: !PDFFloat
                            , previous :: Maybe (Int,Int,Int,BreakNode)
                            }
                            deriving(Show)
                            
                            
dilatationRatio :: PDFFloat -- ^ Maxw
                -> PDFFloat -- ^ Current w
                -> PDFFloat -- ^ y
                -> PDFFloat -- ^ z
                -> PDFFloat -- ^ Dilatation ratio
dilatationRatio maxw w y z =                 
                if w == maxw 
                    then 0.0
                    else if w < maxw
                      then
                          if y > 0.0 then  ((maxw - w) / y) else bigAdjustRatio
                      else
                          if z > 0.0 then  ((maxw - w) / z) else bigAdjustRatio
               
                            
adjustRatio :: BreakNode
            -> ZList s
            -> PDFFloat
            -> PDFFloat
adjustRatio a l maxw = 
    let w = cumulatedW l - totalWidth a + penaltyWidth (letter l)
        y = cumulatedY l - totalDilatation a
        z = cumulatedZ l - totalCompression a
    in
    dilatationRatio maxw w y z

badness :: PDFFloat -> PDFFloat
badness r = if r < (-1) then bigAdjustRatio else 100.0 * abs(r)**3.0

fitness ::  PDFFloat -> Int
fitness r = 
 if   r < (-0.5)
  then 
      0
  else if r <= (-0.5)
      then
          1
      else
          if r <= 1 
              then
                  2
              else
                  3
                 
-- | Breaking algorithm settings 
data BRState = BRState { firstPassTolerance :: !PDFFloat -- ^ Default value 100
                       , secondPassTolerance :: !PDFFloat -- ^ Default value 100
                       , hyphenPenaltyValue :: !Int -- ^ Default value 50
                       , fitness_demerit :: !PDFFloat -- ^ Default value 1000
                       , flagged_demerit :: !PDFFloat -- ^ Default value 1000
                       , line_penalty :: !PDFFloat -- ^ Default value 10
                       , centered :: !Justification -- ^ Default value false
                       , hyphenation :: !HyphenationDatabase -- ^ Default value English
                       }
                       
defaultBreakingSettings :: BRState
defaultBreakingSettings = BRState 100 100 50 1000 1000 10 FullJustification (English Nothing)
                  


computeDemerit :: Bool
               -> BRState
               -> Bool
               -> PDFFloat -- ^ adjust ratio
               -> BreakNode -- ^ Flag for previous
               -> ZList s -- ^ Flag for current
               -> Maybe(PDFFloat,Int) -- ^ Demerit for the breakpoint
computeDemerit force settings sndPass r a z =
    let b = badness r
        p = getPenalty z 
        fitness' = fitness r
        tolerance = if sndPass then (secondPassTolerance settings) else (firstPassTolerance settings)
        in
    if (b <= tolerance) || force -- sndPass
        then 
            let fld = if isFlagged z && (flagged a) then (flagged_demerit settings) else 0.0
                fid = if fitness' /= (fitnessValue a) then (fitness_demerit settings) else 0.0
                dem = max 1000.0 $ if p >= 0
                            then
                                fid + fld + ((line_penalty settings) + b) ** 2.0 + (fromIntegral p) ** 2.0
                            else if p < 0 && p > (-infinity)
                                then
                                    fid + fld + ((line_penalty settings) + b) ** 2.0 - (fromIntegral p)**2.0
                                else
                                    fid + fld + ((line_penalty settings) + b) ** 2.0
            in
               Just (dem,fitness')
        else
             Nothing    

data MaybeCB a = NoCB 
               | OneCB !(CB a)
               deriving(Show)

data ZList s = ZList (MaybeCB (Letter s)) (PDFFloat,PDFFloat,PDFFloat,Int,Letter s) [Letter s] deriving(Show)

-- Used for debugging only
--currentLetter :: ZList s -> Letter s
--currentLetter (ZList _ (_,_,_,_,a) _) = a

createZList :: [Letter s] -> ZList s
createZList [] = error "List cannot be empty to create a zipper"
createZList l = ZList NoCB (0,0,0,1,head l) (tail l)

theEnd :: ZList s -> Bool
theEnd (ZList _ _ []) = True
theEnd _ = False

-- | We create a new breakpoint but we get the cumulated dimensions only at the next box following the break
-- since glues and penalties are removed at the beginning of a line
createBreaknode :: Maybe (Int,Int,Int,BreakNode) -> ZList s -> BreakNode
createBreaknode prev a@(ZList _ (_,_,_,_,FlaggedPenalty _ _ _) []) = breakN prev  True a
createBreaknode prev a@(ZList _ (_,_,_,_,Penalty _) []) = breakN prev False a
createBreaknode prev a@(ZList _ (_,_,_,_,Glue _ _ _ _) []) = breakN prev False a
createBreaknode prev a@(ZList _ (_,_,_,_,_) []) = breakN prev False a
createBreaknode prev a@(ZList _ (_,_,_,_,FlaggedPenalty _ p _) _) | p <= infinity = breakN prev True a
createBreaknode prev a@(ZList _ (_,_,_,_,Letter _ _ _) _) = breakN prev False a
createBreaknode prev a@(ZList _ (_,_,_,_,AChar _ _ _) _) = breakN prev False a
createBreaknode prev a@(ZList _ (_,_,_,_,Kern _ _) _) = breakN prev False a
createBreaknode prev z = 
    let BreakNode a b c d _ e f g = createBreaknode prev (moveRight z) in
    BreakNode a b c d False e f g

breakN :: Maybe (Int,Int,Int,BreakNode)  -> Bool -> ZList s -> BreakNode
breakN prev t a =  let (w,y,z) = getDim a in BreakNode w y z 0.0 t 0 0.0 prev

-- | Get cumulated dimension for following box
getDim :: ZList s -> (PDFFloat,PDFFloat,PDFFloat)
getDim (ZList _ (w,y,z,_,Letter _ _ _) _) =  (w,y,z)
getDim (ZList _ (w,y,z,_,AChar _ _ _) _) =  (w,y,z)
getDim (ZList _ (w,y,z,_,Kern _ _) _) =  (w,y,z)
getDim (ZList _ (w,y,z,_,_) []) = (w,y,z)
getDim a = if theEnd a then error "Can't find end of paragraph" else getDim (moveRight a)


moveRight :: ZList s -> ZList s
moveRight (ZList _ c@(w,y,z,p,Glue w' y' z' _) r) = 
    let w'' = w + w'
        y''=y+y'
        z''=z+z'
    in
    ZList (OneCB c) (w'',y'',z'',p+1,head r) (tail r)
moveRight (ZList _ c@(w,y,z,p,a) r) = 
        let w' = glueSizeWithRatio a 0.0
            w'' = w + w'
        in
        ZList (OneCB c) (w'',y,z,p+1,head r) (tail r)
     

isFeasibleBreakpoint :: Bool -- ^ Second pass
                     -> ZList s -- ^ Current analyzed box
                     -> Bool -- ^ Result
isFeasibleBreakpoint True (ZList _ (_,_,_,_,FlaggedPenalty _ p _) _) = p < infinity
isFeasibleBreakpoint False (ZList _ (_,_,_,_,FlaggedPenalty _ _ _) _) = False
isFeasibleBreakpoint _ (ZList _ (_,_,_,_,Penalty p) _) = p < infinity
isFeasibleBreakpoint _ (ZList NoCB _ _) = False
isFeasibleBreakpoint _ (ZList (OneCB (_,_,_,_,Letter _ _ _)) (_,_,_,_,Glue _ _ _ _) _) = True
isFeasibleBreakpoint _ (ZList (OneCB (_,_,_,_,AChar _ _ _)) (_,_,_,_,Glue _ _ _ _) _) = True
isFeasibleBreakpoint _ _ = False




-- Update a feasible breakpoint remembering only the best one
type PossibleBreak = ActiveNodes -- Line, demerit and break node
type ActiveNodes  = Map.Map (Int,Int,Int) BreakNode

updateBreak :: BreakNode
            -> BreakNode
            -> BreakNode
updateBreak a b = if demerit a < demerit b then a else b  

-- | Check is a break point is possible
-- otherwise, if none is possible and there is only one remaining active point,
-- we force a breakpoint
updateWithNewRIfNoSolution :: Bool
                           -> PDFFloat -- ^ Old r
                           -> ZList s -- ^ Current
                           -> (Int,Int,Int)
                           -> PossibleBreak
                           -> ActiveNodes-- ^ Actives
                           -> (Bool -> PDFFloat -> ActiveNodes -> (PossibleBreak,ActiveNodes))
                           -> (PossibleBreak,ActiveNodes)
updateWithNewRIfNoSolution sndPass r z key newbreak newmap f =
        if isForcedBreak z
          then
             f True r (Map.delete key newmap)
          else
             if r < -1 
                then let m' = Map.delete key newmap 
                     in
                     if Map.null m' && sndPass then f True (-0.99) m' else (newbreak,m')
                else
                     f False r newmap   
                        
--debug b z = "(" ++
--    show (cumulatedW z - totalWidth b + penaltyWidth (letter z)) ++ " " ++
--    show (cumulatedY z - totalDilatation b) ++ " " ++
--    show (cumulatedZ z - totalCompression b) ++ ")"   
--    
--breakTrace sndPass b z r' p = trace ("SndPass :" ++ show sndPass ++ " " 
--   ++ debug b z ++ " " ++ show r' ++ " " 
--   ++ show (currentLetter z) ++ " " 
--   ++ show (position z) ++ " -> " 
--   ++ show p
--   ++ if isFlagged z then " (Flagged)" else ""
--   )          

getNewActiveBreakpoints ::  BRState -> Bool -> (Int -> PDFFloat) -> ActiveNodes -> ZList s -> (PossibleBreak,ActiveNodes)
getNewActiveBreakpoints settings sndPass fmaxw actives z = 
    if isFeasibleBreakpoint sndPass z
    then
        let analyzeActive key@(p,line,f) b (newbreak,newmap') = 
              let r' = adjustRatio b z (fmaxw (line+1))
              in -- breakTrace sndPass b z r' p $
              updateWithNewRIfNoSolution sndPass r' z key newbreak newmap' $
               \force r newmap -> let dem' = computeDemerit force settings sndPass r b z in
                        case dem' of
                            Nothing -> (newbreak,newmap)
                            Just (d',f') -> 
                                      let  b' = createBreaknode (Just (p,line,f,b)) z in
                                      -- We keep only the best new break
                                      (Map.insertWith updateBreak (position z,line+1,f') (b' {demerit = d',fitnessValue = f', ratio = r}) newbreak ,newmap)
        in
        let (breaks',actives') = Map.foldWithKey analyzeActive (Map.empty,actives) actives
            dmin = minimum . map demerit . Map.elems $ breaks'
            nbreaks = Map.filter (\x -> demerit x < dmin + (fitness_demerit settings)) breaks'
        in
        if Map.null nbreaks
         then
           (breaks' , actives') 
         else
           (nbreaks , actives') 
    else
       (Map.empty,actives )
          
-- (position, line, fitness) -> (adjust ratio, break position)
genNodeList :: (Int,Int,Int,BreakNode) -> [(PDFFloat,Int,Bool)]
genNodeList (p,_,_,b@(BreakNode _ _ _ _ f _ _ Nothing)) = [(ratio b,p,f)]
genNodeList (p,_,_,b@(BreakNode _ _ _ _ f _ _ (Just _))) = (ratio b,p,f):genNodeList (fromJust . previous $ b)


-- Analyze the boxes to compute breaks
analyzeBoxes :: BRState -> Bool -> (Int -> PDFFloat) -> ActiveNodes -> ZList s -> ZList s -> [(PDFFloat,Int,Bool)]
analyzeBoxes settings pass fmaxw actives lastz z = 
    let getMinBreak b' = (\((xc,yc,zc),w) -> (xc,yc,zc,w)) . minimumBy (\(_,a) (_,b) -> compare (demerit a) (demerit b)) . Map.toList $ b'
        (breaks',actives') = getNewActiveBreakpoints settings pass fmaxw actives z 
        newActives = Map.union (breaks') (actives')
        getRightOrderNodeList = tail . reverse . genNodeList
        getKey (a,b,c,_) = (a,b,c)
        getNode (_,_,_,BreakNode a b c d e f r _) = BreakNode a b c d e f r Nothing
    in
    --  If forced breakpoint or no breakpoint found
    if Map.null actives'
        then
            -- If no breakpoint found
            if Map.null breaks'
                then
                    -- Second pass analysis
                    if not pass
                     then
                          analyzeBoxes settings True fmaxw actives lastz lastz
                     else
                          error "Second pass analysis failed ! Generally due to wrong width in the text area or an end of text before end of paragraph detected"
                else 
                    -- Forced breakpoint then we gen a list from it and continue the analysis
                    let minBreak = getMinBreak breaks' 
                        someNewBreaks = getRightOrderNodeList minBreak
                    in
                    if theEnd z
                      then
                        someNewBreaks
                      else
                        let z' = moveRight z in
                        someNewBreaks ++ analyzeBoxes settings pass fmaxw (Map.insert (getKey minBreak) (getNode minBreak) Map.empty) z' z'
        -- Normal feasible breakpoint
        else
            if Map.null breaks'
             then
                 if theEnd z
                    then  
                        error "End of text found but no paragraph end detected"
                    else
                        analyzeBoxes settings pass fmaxw actives' lastz (moveRight z)
             else
                -- If the end it must be a possible breakpoint
                -- We should NEVER reach this part. The end should always be a forced breakpoint
                if theEnd z
                   then
                     let minBreak = getMinBreak breaks' in
                     getRightOrderNodeList minBreak
                   else
                     -- We continue the analysis
                     analyzeBoxes settings pass fmaxw newActives lastz (moveRight z)

-- | Create an hyphen box
hyphenBox :: Style s => s -> Letter s
hyphenBox s = AChar s '-' (charWidth (textFont . textStyle $ s) '-')

-- Use a list of breakpoint and adjustement ratios to generate a list of lines. Bool means if the break was done on a flagged penalty (hyphen)
cutList :: Style s => Justification -> [Letter s] -> Int -> [(PDFFloat,Int,Bool)] -> [(PDFFloat,[Letter s],[Letter s])]
cutList _ [] _ _ = []
cutList _ t _ [] = [(0.0,[],t)]
cutList j t c ((ra,ba,fa):l) = 
   let (theLine,t') = splitAt (ba-c) t 
   in
   if null theLine 
      then
         []
      else 
         if null t'
             then
                 [(ra,theLine,t)]
             else
                 case head t' of
                     FlaggedPenalty _ _ s -> if not fa 
                                               then
                                                  error $ "Breakpoint marked as not flagged but detected as flagged ! Send a bug report ! " ++ show (ra,ba,fa)
                                               else
                                                 (ra,theLine ++ hyphenForJustification j s,t) : cutList j t' ba l
                     _ -> if fa 
                            then
                                error $ "Breakpoint marked as flagged but detected as not flagged ! Send a bug report ! " ++ show (ra,ba,fa) ++ " " ++ show theLine ++ " " ++ show t'
                            else
                                (ra,theLine,t) : cutList j t' ba l
     
-- Compute the breakpoints and generate a list of lines with the adjustement ratios
-- The line are not interpreted. Some additional postprocessing is required
-- for horizontal lines of vertical boxes
formatList :: Style s => BRState -> (Int -> PDFFloat) -> [Letter s] -> [(PDFFloat,[Letter s],[Letter s])]
formatList settings maxw boxes = 
    let active = Map.insert (0,0,1) (BreakNode 0 0 0 0 False 0 0.0 Nothing) Map.empty
        z = createZList boxes
        theBreaks = analyzeBoxes settings False maxw active z z
    in
    cutList (centered settings) boxes 1 theBreaks
     
-- | Value modeling infinity  
infinity :: Int 
infinity = 10000

bigAdjustRatio :: PDFFloat
bigAdjustRatio = 10000.0

-- | Add a glue to the stream
glueBox :: Maybe s
        -> PDFFloat -- ^ Glue width
        -> PDFFloat -- ^ Glue dilatation
        -> PDFFloat -- ^ Glue compression
        -> Letter s
glueBox s w y z = Glue w y z s

-- | Return the standard space width
spaceWidth :: Style s => s -- ^ The style
           -> PDFFloat
spaceWidth  s =  
    let ws = (textWidth (textFont . textStyle $ s) (toPDFString " ") )
        h = scaleSpace . textStyle $ s
    in
      ws * h  
   
-- | How much dilatation is allowed compred to the space width   
centeredDilatationFactor :: PDFFloat
centeredDilatationFactor = 10.0       

-- | How much dilatation is allowed compared to the space width   
leftDilatationFactor :: PDFFloat
leftDilatationFactor = 20.0

-- | How much dilatation is allowed compared to the space width   
rightDilatationFactor :: PDFFloat
rightDilatationFactor = 20.0

-- | Add a glue to the stream
spaceGlueBox :: Style s => BRState -- ^ Paragraph settings
             -> s -- ^ The style
             -> PDFFloat
             -> [Letter s]
spaceGlueBox settings s f = 
     let ws = (textWidth (textFont . textStyle $ s) (toPDFString " ") )
         h = scaleSpace . textStyle $ s
         sy = scaleDilatation . textStyle $ s
         sz = scaleCompression . textStyle $ s
         normalW = ws * h
     in
     case (centered settings) of
        FullJustification -> [Glue (normalW) (normalW*sy/2.0*f) (normalW*sz/3.0) (Just s)]
        Centered -> [ Glue 0 (centeredDilatationFactor*normalW) 0 (Just s)
                    , Penalty 0
                    , Glue (normalW) (-2*centeredDilatationFactor*normalW) 0 (Just s)
                    , Kern 0 (Just s)
                    , Penalty infinity
                    , Glue 0 (centeredDilatationFactor*normalW) 0 (Just s)
                    ]
        LeftJustification -> [ Glue 0 (leftDilatationFactor*normalW) 0 (Just s)
                             , Penalty 0
                             , Glue normalW (-leftDilatationFactor*normalW) 0 (Just s)
                             ] 
        RightJustification -> [ Glue normalW (-rightDilatationFactor*normalW) 0 (Just s)
                              , Kern 0 (Just s)
                              , Penalty infinity
                              , Glue 0 (rightDilatationFactor*normalW) 0 (Just s)
                              ]                    
spaceSize ::  Style s => s -- ^ The style 
          -> PDFFloat                       
spaceSize s =
     let ws = (textWidth (textFont . textStyle $ s) (toPDFString " ") )
         h = scaleSpace . textStyle $ s
     in  ws * h
     
-- | When a paragraph is full and we start a new one we must clean the beginning paragraph and remove what has been left by the
-- broken space
simplify :: [Letter s]
         -> [Letter s]
simplify [] = []
simplify ((Glue _ _ _ _):l) = simplify l
simplify ((FlaggedPenalty _ _ _):l) = simplify l
simplify ((Penalty _):l) = simplify l                
simplify l = l
                                   
hyphenForJustification :: Style s => Justification -> s -> [Letter s]
hyphenForJustification Centered s = [hyphenBox s,Glue 0 (centeredDilatationFactor*spaceSize s) 0 (Just s)]
hyphenForJustification LeftJustification s = [hyphenBox s,Glue 0 (leftDilatationFactor*spaceSize s) 0 (Just s)]
hyphenForJustification _ s = [hyphenBox s]
                        
                          
-- | Add a penalty to the stream
penalty :: Int -- ^ Penalty value
        -> Letter s
penalty p = Penalty p

-- | Create a box containing text
createChar :: s -- ^ Char style
           -> PDFFloat -- ^ Char width
           -> Char -- ^ Char code
           -> Letter s
createChar s w t = AChar s t w


-- | Create boxes for the letters
createLetterBoxes :: Style s => BRState
                  -> s -- ^ Letter style
                  -> [(PDFFloat,Char)] -- ^ Letter and size 
                  -> [Letter s] -- ^ Boxes
createLetterBoxes _ _ [] = []  
createLetterBoxes settings s ((_,'/'):(w,'-'):l) = hyphenPenalty settings s w : createLetterBoxes settings s l
createLetterBoxes settings s ((w',','):(_,' '):l) = (createChar s w' ',') : ((spaceGlueBox settings s 2.0) ++ createLetterBoxes settings s l)
createLetterBoxes settings s ((w',';'):(_,' '):l) = (createChar s w' ';') : ((spaceGlueBox settings s 2.0) ++ createLetterBoxes settings s l)
createLetterBoxes settings s ((w','.'):(_,' '):l) = (createChar s w' '.') : ((spaceGlueBox settings s 2.0) ++ createLetterBoxes settings s l)
createLetterBoxes settings s ((w',':'):(_,' '):l) = (createChar s w' ':') : ((spaceGlueBox settings s 2.0) ++ createLetterBoxes settings s l)
createLetterBoxes settings s ((w','!'):(_,' '):l) = (createChar s w' '!') : ((spaceGlueBox settings s 2.0) ++ createLetterBoxes settings s l)
createLetterBoxes settings s ((w','?'):(_,' '):l) = (createChar s w' '?') : ((spaceGlueBox settings s 2.0) ++ createLetterBoxes settings s l)
createLetterBoxes settings s ((_,' '):l) = (spaceGlueBox settings s 1.0) ++ createLetterBoxes settings s l
createLetterBoxes settings s ((w,t):l) = (createChar s w t) : createLetterBoxes settings s l

-- | split a line into boxes and add hyphen where needed
splitText :: Style s => BRState -> s -> PDFString -> [Letter s]
splitText settings f t  = wordToLetters t
  where
     wordToLetters = createLetterBoxes settings f . ripText (textFont . textStyle $ f)
     
-- | Create an hyphen penalty
hyphenPenalty :: BRState
              -> s -- ^ Style of future hyphen
              -> PDFFloat -- ^ Size of hyphen taking into account the kerning that was perturbed by the hyphen introduction. The char before the hyphen is now bigger
              -> Letter s
hyphenPenalty settings s w = FlaggedPenalty w (hyphenPenaltyValue settings) s

kernBox :: s -> PDFFloat -> Letter s
kernBox s w = Kern w (Just s)