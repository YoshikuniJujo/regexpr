-- RegexPRTypes.hs
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

module Hidden.RegexPRTypes (
  reverseRegexAction
, RegexSrcParser
, getBR
, modifyBR
, isModeI
, isModeM
, isModeX
, setMode
, setModes
, getModes
, runRegexSrcParser
, RegexAction(..)

, RegexResult
, VerboseResult
, MatchList
, RegexParser
, runRegexParser

, MatchFun
, VerboseMatchFun
) where

import Text.ParserCombinators.MTLParse ( Parse, runParse )
import Control.Monad.State             ( StateT, runStateT, gets, modify )
import Control.Monad.Reader            ( ReaderT(runReaderT) )
import Control.Arrow                   ( first, second )

type RegexResult = ( String, (String, String) )
type MatchList   = [ (Int, String) ]
type RegexParser = ReaderT (String, String) (StateT MatchList (Parse Char))
runRegexParser ::
  (String, String) ->
  RegexParser a -> (String, String) -> [((a, MatchList), (String, String))]
runRegexParser point = runParse . flip runStateT [] . flip runReaderT point

type Modes = String
type RegexSrcParser = StateT (Int, Modes) (Parse Char)

getBR :: RegexSrcParser Int
getBR    = gets fst

modifyBR :: (Int -> Int) -> RegexSrcParser ()
modifyBR = modify . first

setMode :: Char -> Bool -> RegexSrcParser ()
setMode c True  = modify $ second (c:)
setMode c False = modify $ second (filter (/=c))

getModes :: RegexSrcParser Modes
getModes = gets snd
setModes :: Modes -> RegexSrcParser ()
setModes ms = modify $ second $ const ms

isModeI, isModeM, isModeX :: RegexSrcParser Bool
isModeI = gets $ elem 'i' . snd
isModeM = gets $ elem 'm' . snd
isModeX = gets $ elem 'x' . snd
runRegexSrcParser :: RegexSrcParser a -> Parse Char (a, (Int,String))
runRegexSrcParser = flip runStateT (1, "")

data RegexAction = Select (Char -> Bool)                           |
                   Repeat          Int (Maybe Int) RegexAction     |
                   RepeatNotGreedy Int (Maybe Int) RegexAction     |
		   RegexOr [RegexAction] [RegexAction]             |
                   Note Int [RegexAction] | BackReference Int      |
		   Still [RegexAction]    | Backword [RegexAction] |
		   RegActNot [RegexAction]                         |
		   BeginningOfInput       | EndOfInput             |
		   PreMatchPoint          | Parens [RegexAction]   |
		   Comment String         | NopRegex               |
		   NoBacktrack [RegexAction]

reverseRegexAction :: RegexAction -> RegexAction
reverseRegexAction (Note i ras)
  = Note i $ reverse $ map reverseRegexAction ras
reverseRegexAction (Parens ras)
  = Parens $ reverse $ map reverseRegexAction ras
reverseRegexAction (RegexOr ras1 ras2)
  = RegexOr (reverse $ map reverseRegexAction ras1)
            (reverse $ map reverseRegexAction ras2)
-- reverseRegexAction (Still ras)
--  = Still $ reverse $ map reverseRegexAction ras
-- reverseRegexAction (Backword ras)
--  = Backword $ reverse $ map reverseRegexAction ras
reverseRegexAction ra = ra

type MatchFun f
  = String -> String -> f ( RegexResult, MatchList )

type VerboseResult = ( String, String, (String, String) )
type VerboseMatchFun f
  = String -> (String, String) -> f ( VerboseResult, MatchList )
