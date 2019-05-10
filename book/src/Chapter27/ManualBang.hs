{-# OPTIONS_GHC -ddump-simpl #-}
{-# OPTIONS_GHC -dsuppress-all #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE BangPatterns #-}

module Chaper27.ManualBang where

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

banging :: Bool -> Int
banging !b = 1

-- Bang patterns in data
data Foo = Foo Int !Int

first :: Foo -> Int
first (Foo x _) = x

second :: Foo -> Int
second (Foo _ y) = y

second (Foo undefined 1)

-- first (Foo 1 undefined)  -- Exception: Prelude.undefined
