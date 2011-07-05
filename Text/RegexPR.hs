--
-- RegexPR.hs
--
-- Author: Yoshikuni Jujo <PAF01143@nifty.ne.jp>
--
-- This file is part of regexpr library
--
-- regexpr is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or any later version.
--
-- regexpr is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANGY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this program. If not, see
-- <http://www.gnu.org/licenses/>.

module Text.RegexPR (

  matchRegexPR
, multiMatchRegexPR
, gmatchRegexPR

, getbrsRegexPR
, ggetbrsRegexPR

, subRegexPR
, subRegexPRBy
, gsubRegexPR
, gsubRegexPRBy

, splitRegexPR

) where

import Hidden.RegexPRCore  ( matchRegexPRVerbose,
                             multiMatchRegexPRVerbose          )
import Hidden.RegexPRTypes ( MatchFun   , VerboseMatchFun,
                             RegexResult, VerboseResult  ,
			     MatchList                         )
import Data.Char           ( isDigit                           )
import Data.List           ( sort, nubBy                       )
import Data.Function       ( on                                )
import Data.Maybe          ( fromMaybe                         )
import Control.Arrow       ( first                             )

------------------------------------------------------------

matchRegexPR      :: MatchFun Maybe
matchRegexPR      = simplifyMatchFun matchRegexPRVerbose

multiMatchRegexPR :: MatchFun []
multiMatchRegexPR = simplifyMatchFun multiMatchRegexPRVerbose

gmatchRegexPR :: MatchFun []
gmatchRegexPR reg = baseFun . (,) ""
  where
  baseFun ( _, "" ) = []
  baseFun pos       = maybe [] justFun $ matchRegexPRVerbose reg pos
  justFun mr@( ( _, r, pos ), _ )
    = first simplifyResult mr :
      baseFun ( if null r then next pos else pos )
  next ( p, x:xs ) = ( x:p, xs )
  next _           = error "can not go to next"

simplifyMatchFun :: Functor f => VerboseMatchFun f -> MatchFun f
simplifyMatchFun mf reg
  = fmap ( first simplifyResult ) . mf reg . (,) ""

simplifyResult :: VerboseResult -> RegexResult
simplifyResult ( pre, ret, (_, rest) ) = ( ret, (pre, rest) )

------------------------------------------------------------

getbrsRegexPR :: String -> String -> [ String ]
getbrsRegexPR reg str
  = case matchRegexPR reg str of
         Nothing
	   -> []
	 Just ( ( ret, (_, _) ), ml )
	   -> ret : map snd ( sort $ nubBy ( on (==) fst ) ml )

ggetbrsRegexPR :: String -> String -> [ [ String ] ]
ggetbrsRegexPR reg
  = map ( \( (m, _), bl ) ->
            m : map snd ( sort $ nubBy (on (==) fst) bl ) )
    . gmatchRegexPR reg

------------------------------------------------------------

splitRegexPR :: String -> String -> [String]
splitRegexPR reg str
  = case gmatched of
         [ ] -> [ ]
         _   -> map ( fst.snd.fst ) gmatched ++ [ (snd.snd.fst.last) gmatched ]
  where gmatched = gmatchRegexPR reg str

------------------------------------------------------------

subRegexPR :: String -> String -> String -> String
subRegexPR reg sub = subRegexPRBy reg (const sub)

subRegexPRBy :: String -> (String -> String) -> String -> String
subRegexPRBy reg subf src
  = case matchRegexPRVerbose reg ("",src) of
         Just al@((pre, m, sp), _) -> pre ++ subBackRef al (subf m) ++ snd sp
         Nothing                   -> src

gsubRegexPR :: String -> String -> String -> String
gsubRegexPR reg sub src = gsubRegexPRGen Nothing reg (const sub) ("", src)

gsubRegexPRBy :: String -> (String -> String) -> String -> String
gsubRegexPRBy reg subf src = gsubRegexPRGen Nothing reg subf ("", src)

gsubRegexPRGen ::
  Maybe (String, String) -> String -> (String -> String) -> (String, String) -> String
gsubRegexPRGen pmp reg fsub src
  = case matchRegexPRVerbose reg src of
      Just al@((pre, match, sp@(~(p,x:xs))), _)
        -> case (pmp, sp) of
                (Just (_, ""), _)  -> ""
                _ | Just sp == pmp -> pre ++ [x] ++
                                      gsubRegexPRGen (Just sp) reg fsub (x:p, xs)
                  | otherwise      -> pre ++ subBackRef al (fsub match) ++
                                      gsubRegexPRGen (Just sp) reg fsub sp
      Nothing -> snd src

subBackRef ::
  ((String, String, (String, String)), MatchList) -> String -> String
subBackRef (_, _) "" = ""
subBackRef al@((_, match, (hasRead,post)), ml) ('\\':str@(c:rest))
  | c `elem` "&0" = match                                 ++ subBackRef al rest
  | c == '`'    = reverse (drop (length match) hasRead) ++ subBackRef al rest
  | c == '\''   = post                                  ++ subBackRef al rest
  | c == '+'    = snd (head ml)                         ++ subBackRef al rest
  | c == '{'    = fromMaybe "" (lookup (read $ takeWhile (/='}') rest) ml) ++
                  subBackRef al (tail $ dropWhile (/='}') str)
  | otherwise   = fromMaybe "" (lookup (read $ takeWhile isDigit str) ml) ++
                  subBackRef al (dropWhile isDigit str)
subBackRef al (c:cs) = c : subBackRef al cs
