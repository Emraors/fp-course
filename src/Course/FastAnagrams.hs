{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S
import Course.ListZipper (fromList)


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"



anag :: Chars -> List Chars -> List Chars
anag name dictionary = listh . S.toList $
  S.intersection (S.fromList . hlist . permutations $ name) (S.fromList . hlist $ dictionary)


fastAnagrams :: Chars -> FilePath -> IO (List Chars)
fastAnagrams name file = anag name . lines <$> readFile file

newtype NoCaseString = NoCaseString Chars

ncString :: NoCaseString  -> Chars
ncString (NoCaseString s) = s

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString


instance Show NoCaseString where
  show = show . ncString
