-- SrcRegActList.hs
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

module Hidden.SrcRegActList (
  selfTest
, plusesList
, oneCharList
, backSlashesList
, parensesList
, charClassList
) where

import Hidden.RegexPRTypes ( RegexAction(..), reverseRegexAction )
import Data.Char           ( isAlphaNum, isAlpha, isUpper, isLower,
                             isDigit, isHexDigit, isSpace, isPrint, isControl )
import Hidden.Tools        ( (&&&), (|||), isSymbol, isBit7On )

selfTest :: Char -> Bool
selfTest
  = flip elem "\b\n\f\r\t !\"#%&',-/:;<=>@]_'~}" ||| isAlphaNum ||| isBit7On

plusesList :: [ (String, RegexAction -> RegexAction) ]
plusesList = [
   ( ""  , id )
 , ( "*" , Repeat          0 Nothing  )
 , ( "*?", RepeatNotGreedy 0 Nothing  )
 , ( "+" , Repeat          1 Nothing  )
 , ( "+?", RepeatNotGreedy 1 Nothing  )
 , ( "?" , Repeat          0 $ Just 1 )
 , ( "??", RepeatNotGreedy 0 $ Just 1 )
 ]

oneCharList :: [ (Char, RegexAction) ]
oneCharList = [
   ('.', Select (/='\n'))
 , ('$', RegexOr [EndOfInput] [Still [Select (=='\n')]])
 , ('^', RegexOr [BeginningOfInput] [Still [Backword [Select (=='\n')]]])
 ]

backSlashesList :: [ (Char, RegexAction) ]
backSlashesList = [
   ( 'w', Select isWord                                              )
 , ( 'W', Select $ not . isWord                                      ) 
 , ( 's', Select (`elem` " \t\n\r\f")                             )
 , ( 'S', Select (`notElem` " \t\n\r\f")                          )
 , ( 'd', Select isDigit                                             )
 , ( 'D', Select (not . isDigit)                                     )
 , ( 'A', BeginningOfInput                                           )
 , ( 'Z', RegexOr [EndOfInput] [Still [Select (=='\n'), EndOfInput]] )
 , ( 'z', EndOfInput                                                 )
 , ( 'b', regexOr [
            [ lookBehind [Select    isWord], Still [selectNot isWord] ]
	  , [ lookBehind [selectNot isWord], Still [Select    isWord] ]
	  , [ BeginningOfInput,              Still [Select    isWord] ]
	  , [ lookBehind [Select    isWord], EndOfInput               ]
	  ]                                                          )
 , ( 'B', RegActNot [ regexOr [
            [ lookBehind [Select    isWord], Still [selectNot isWord] ]
	  , [ lookBehind [selectNot isWord], Still [Select    isWord] ]
	  , [ BeginningOfInput,              Still [Select    isWord] ]
	  , [ lookBehind [Select    isWord], EndOfInput               ]
	  ] ]                                                        )
 , ( 'G', PreMatchPoint                                              )
 ]

parensesList :: [ (String, [RegexAction] -> RegexAction) ]
parensesList = [
   ( "?<=", Still . (:[]) . Backword . reverse . map reverseRegexAction )
 , ( "?<!", Still . (:[]) . RegActNot . (:[]) . Backword . reverse
                                              . map reverseRegexAction  )
 , ( "?=",  Still                                                       )
 , ( "?!",  Still . (:[]) . RegActNot                                   )
 , ( "?:",  Parens                                                      )
 , ( "?>",  NoBacktrack                                                 )
 , ( "$>",  NoBacktrack                                                 )
 ]

charClassList :: [ (String, Char -> Bool) ]
charClassList = [
   ( "alnum" , isAlphaNum                 )
 , ( "alpha" , isAlpha                    )
 , ( "blank" , isSpace                    )
 , ( "cntrl" , isControl                  )
 , ( "digit" , isDigit                    )
 , ( "graph" , isPrint &&& (not.) isSpace )
 , ( "lower" , isLower                    )
 , ( "print" , isPrint                    )
 , ( "punct" , isSymbol                   )
 , ( "space" , isSpace                    )
 , ( "upper" , isUpper                    )
 , ( "xdigit", isHexDigit                 )
 ]

isWord :: Char -> Bool
isWord = isAlphaNum ||| (=='_')

regexOr :: [[RegexAction]] -> RegexAction
regexOr = head . foldr1 (\ra1 ra2 -> [RegexOr ra1 ra2])

lookBehind :: [RegexAction] -> RegexAction
lookBehind ras = Still [Backword ras]

selectNot :: (Char -> Bool) -> RegexAction
selectNot s = Select (not . s)
