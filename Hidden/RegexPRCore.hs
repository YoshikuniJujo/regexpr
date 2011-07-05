-- RegexPRCore.hs
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

module Hidden.RegexPRCore (
  matchRegexPRVerbose
, multiMatchRegexPRVerbose
) where

import Hidden.RegexPRTypes  ( RegexParser, MatchList, runRegexParser )
import Text.ParserCombinators.MTLParse
                            ( spot, spotBack, still, noBacktrack, parseNot,
                              build, tokens, tokensBack,
                              repeatParse, greedyRepeatParse,
                              beginningOfInput, endOfInput,
                              MonadPlus(..), (>++>) )
import Hidden.ParseRegexStr ( RegexAction(..), parseRegexStr )
import Control.Monad.State  ( StateT, runStateT, gets, modify, lift, liftM )
import Control.Monad.Reader ( ask )
import Hidden.Tools         ( guardEqual )
import Control.Monad        ( unless )

matchRegexPRVerbose ::
  String -> (String, String)
         -> Maybe ( (String, String, (String, String)), MatchList )
matchRegexPRVerbose reg str
  = case (runRegexParserTrials . mkRegexParserTrials . parseRegexStr) reg str of
         []                       -> Nothing
         (((ret, pre), ml), sp):_ -> Just ( (reverse pre, ret, sp), ml )

multiMatchRegexPRVerbose ::
  String -> (String, String)
         -> [ ( (String, String, (String, String)), MatchList ) ]
multiMatchRegexPRVerbose reg str
  = map (\(((ret, pre), ml), sp) -> ((reverse pre, ret, sp), ml)) $
        (runRegexParserTrials . mkRegexParserTrials . parseRegexStr) reg str

runRegexParserTrials ::
  StateT String RegexParser a ->
    (String, String) -> [(((a, String), MatchList), (String, String))]
runRegexParserTrials p point = runRegexParser point (runStateT p "") point

mkRegexParserTrials :: [RegexAction] -> StateT String RegexParser String
mkRegexParserTrials ras
  = lift (mkRegexParser False ras) `mplus`
    do x <- spot $ const True
       modify (x:)
       mkRegexParserTrials ras

mkRegexParser :: Bool -> [RegexAction] -> RegexParser String
mkRegexParser _ [] = return ""
mkRegexParser isBack (ra:ras)
  = case ra of
         Select s          -> selectParserFB s
         Repeat mn mx rb -> liftM concat . greedyRepeatParse mn mx $
                                mkRegexParser isBack [rb]
         RepeatNotGreedy mn mx rb
                           -> liftM concat . repeatParse mn mx $
                                mkRegexParser isBack [rb]
         Note i acts       -> noteParens isBack i $ mkRegexParser isBack acts
         BackReference ri  -> backReference isBack ri
         RegexOr ra1 ra2   -> mkRegexParser isBack ra1 `mplus`
                              mkRegexParser isBack ra2
         EndOfInput        -> endOfInput ""
         BeginningOfInput  -> beginningOfInput ""
         Still [Backword acts]
                           -> still (mkRegexParser True acts)    >>
                              unless isBack (modify reverse) >> return ""
         Still acts        -> still (mkRegexParser False acts)   >> return ""
         Backword acts     -> mkRegexParser True acts
         RegActNot acts    -> parseNot "" $ mkRegexParser isBack acts
         PreMatchPoint     -> guardEqual ask (lift ask)          >> return ""
         Parens acts       -> mkRegexParser isBack acts
         Comment _         -> return ""
         NopRegex          -> return ""
	 NoBacktrack acts  -> noBacktrack $ mkRegexParser isBack acts
    >++> mkRegexParser isBack ras
    where selectParserFB = if isBack then selectParserBack else selectParser

selectParser, selectParserBack :: (Char -> Bool) -> RegexParser String
selectParser     s = spot     s `build` (:[])
selectParserBack s = spotBack s `build` (:[])

noteParens :: Bool -> Int -> RegexParser String -> RegexParser String
noteParens isBack i p = do x <- p
                           modify ((i, (if isBack then reverse else id) x):)
                           return x

backReference :: Bool -> Int -> RegexParser String
backReference isBack i
  = gets (lookup i) >>=
      maybe mzero (if isBack then tokensBack . reverse else tokens)
