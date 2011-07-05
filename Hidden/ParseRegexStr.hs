-- ParseRegexStr.hs
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

module Hidden.ParseRegexStr (
  RegexAction(..)
, parseRegexStr
) where

import Hidden.RegexPRTypes ( RegexAction(..),
                             RegexSrcParser, runRegexSrcParser,
			     getBR, modifyBR,
			     setMode, setModes, getModes,
			     isModeI, isModeM, isModeX )
import Text.ParserCombinators.MTLParse
                           ( runParse, spot, token, tokens, mzero, mplus,
                             still, parseNot, endOfInput, MonadParse,
			     MonadPlus,
                             list, neList, greedyNeList, optional )
import Hidden.Tools	   ( isSymbol, ignoreCase, skipRet, (>..>), ifM,
                             applyIf, (&&&), headOrErr, modifyFst )
import Data.Char	   ( isAlphaNum, isDigit, isSpace )
import Data.Ix             ( inRange )
import Hidden.SrcRegActList( selfTest, oneCharList, backSlashesList, plusesList,
                             parensesList, charClassList )
import Control.Applicative ((<$>))

parseRegexStr :: String -> [RegexAction]
parseRegexStr src =
  fst . fst . headOrErr ("parse error: regex " ++ show src ++ " is uncorrect") .
    runParse ( runRegexSrcParser parseRegexStrParser) . (,) [] $ src

parseRegexStrParser, parseTokensOr, parseTokens :: RegexSrcParser [RegexAction]
parseRegexStrParser = parseTokensOr >>= endOfInput
parseTokensOr = parseTokens
		`mplus`
                do { ra1 <- parseTokens; _ <- token '|'; ra2 <- parseTokensOr;
		     return [ RegexOr ra1 ra2 ] }
parseTokens = list parseTokenPlus

parseTokenPlus, parseToken :: RegexSrcParser RegexAction
parseTokenPlus = do ra   <- parseToken
                    plus <- parsePluses plusesList `mplus` parseQuantifier
		    return $ plus ra
parseQuantifier :: RegexSrcParser (RegexAction -> RegexAction)
parseQuantifier
  = do { _ <- token '{';
         mn <- neList $ spot isDigit;
         mx <- do { cma <- optional $ token ',';
	            case cma of
		         "" -> return Nothing
			 _  -> fmap Just $ list (spot isDigit) };
         _ <- token '}';
	 nd <- fmap null $ optional (token '?');
         return $ (if nd then Repeat else RepeatNotGreedy) (read mn) $
	                            case mx of
	                                 Nothing -> Just $ read mn
					 Just "" -> Nothing
					 Just n  -> Just $ read n }

parseToken
  = ifM isModeX parseTokenX mzero
    `mplus`
    ( isModeI >>= \ic ->
       fmap (Select . applyIf ic ignoreCase . (==)) (spot selfTest) )
    `mplus`
    parseOpenBrace
    `mplus`
    ifM isModeM ( token '.' >> return (Select $ const True) ) mzero
    `mplus`
    fmap (Select . (==)) (token '\\' >> spot isSymbol)
    `mplus`
    parseBackReference
    `mplus`
    ( fmap (not . null) ( token '[' >> optional (token '^') ) >>= \isNot ->
      fmap (Select . applyIf isNot (not.)) (
      parseCharList >>= skipRet (token ']') 
       ) )
    `mplus`
    ( getBR >>= \i -> fmap (Note i) $ token '(' >> modifyBR (+1) >> parseTokensOr
            >>= skipRet (token ')') )
    `mplus`
    ( tokens "(?" >> list parseMode >>= mapM_ (uncurry setMode) >> token ')'
                  >> return NopRegex )
    `mplus`
    ( getModes >>= \preModes ->
      fmap Parens $ 
      tokens "(?" >> list parseMode >>= mapM_ (uncurry setMode) >> token ':' >>
      parseTokensOr >>= skipRet (setModes preModes >> token ')')
                    )
    `mplus`
    parseOneChar oneCharList
    `mplus`
    parseBackSlashes backSlashesList
    `mplus`
    parseParenses parensesList
    `mplus`
    fmap Comment
    ( tokens "(?#" >> list (spot (/=')')) >>= skipRet (token ')') )

parseMode :: RegexSrcParser (Char, Bool)
parseMode =
              fmap ( uncurry (flip (,)) . modifyFst null ) $
            optional (token '-') >..> spot (`elem` "imx")

parseTokenX :: RegexSrcParser RegexAction
parseTokenX
  = ( spot isSpace >> return NopRegex ) `mplus`
    fmap Comment
    ( token '#' >> list (spot (/='\n')) >>=
      skipRet (token '\n' `mplus` endOfInput '\n') )

parsePluses ::
  [ (String, RegexAction -> RegexAction) ] ->
				RegexSrcParser (RegexAction -> RegexAction)
parsePluses = concatMapParse (\(t, act) -> tokens t >> return act)

parseOneChar :: [ (Char, RegexAction) ] -> RegexSrcParser RegexAction
parseOneChar
  = concatMapParse (\(t, act) -> token t >> return act)

parseBackSlashes :: [ (Char, RegexAction) ] -> RegexSrcParser RegexAction
parseBackSlashes
  = concatMapParse (\(t, act) -> tokens ['\\', t] >> return act)

parseParenses ::
  [ (String, [RegexAction] -> RegexAction) ] -> RegexSrcParser RegexAction
parseParenses
  = concatMapParse ( \(t, act) ->
      (fmap act $ tokens ('(':t) >> parseTokensOr >>= skipRet (token ')') ))

parseCharList :: RegexSrcParser (Char -> Bool)
parseCharList = do
  modei <- isModeI
  cl1 <- parseOne `mplus` concatMapParse ((>>= return . (==)) . token) "-]"
  cl2 <- list $ parseOne `mplus` fmap (==) (token '^')
  return $ applyIf modei ignoreCase $ or . zipWith ($) (cl1 : cl2) . repeat
  where parseOne       = fmap (==) parseChar `mplus` parseCharArea
                                                       `mplus` parseCharClass
        parseChar      = spot isAlphaNum                    `mplus`
		         ( token '\\' >> spot isSymbol )        `mplus`
			 spot (selfTest &&& flip notElem "-]" ) `mplus`
			 spot (`elem` ".+$" )                `mplus`
			 ( token '[' >>= skipRet (still $ parseNot ()
			                                $ token ':') )
        parseCharArea  = fmap inRange $ (parseChar >>= skipRet (token '-')) >..> parseChar
	parseCharClass = concatMapParse
	                   (\(s, p) -> tokens ("[:"++s++":]") >> return p)
			   charClassList

concatMapParse :: MonadPlus m => (b -> m a) -> [b] -> m a
concatMapParse f = foldr (mplus . f) mzero

parseOpenBrace :: RegexSrcParser RegexAction
parseOpenBrace = do still $ parseNot () parseQuantifier
                    still $ parseNot () parseBackReference
                    ret <- token '{'
                    return $ Select (==ret)

parseBackReference :: RegexSrcParser RegexAction
parseBackReference = do
  brace <- null <$> optional (token '{')
  _ <- token '\\'
  dgt <- greedyNeList (spot isDigit)
  _ <- if brace then return ' ' else token '}'
  return $ BackReference $ read dgt
