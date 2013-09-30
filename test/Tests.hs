{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Language.Spaceship.Transcompiler hiding (main)
import Language.Haskell.Exts
import System.Process (system)
import GHC.IO.Exception (ExitCode(..))
import Data.Monoid
import Control.Exception (evaluate)
import Data.Char (toLower, toUpper)
import Data.Map as M hiding (map)
import System.Directory (removeFile)

-- At the moment, many of these tests are brittle -- semantically equivalent
--   code is rejected (e.g. ```foo(a + b, c)``` vs ```foo((a + b), c)```).
--   The gold standard is to pass to evaluate the erlang and haskell code
--   and make sure it gets right


main = hspec $ do
   describe "expressions" $ do
      let correspondsToExpr (hsExpr, topLevelFs) erlExpr =
             let (ParseOk m) = parseExp hsExpr
             in (uP (expr ParseContext{topLevels=topLevelFs
                                      ,fArities=M.fromList $
                                                   map (,0) topLevelFs
                                      } m)) `shouldBe` (uP erlExpr)
          correspondsToExprWArities (hsExpr, topLevels, arities) erlExpr=
             let (ParseOk m) = parseExp hsExpr
             in (uP (expr ParseContext{topLevels=topLevels
                                      ,fArities =arities
                                      } m)) `shouldBe` (uP erlExpr)
      describe "$" $ do
         it "'$' in function application" $
            ("foo 30 40 $ 50 + 60", ["foo"])
               `correspondsToExpr` "foo(30, 40, (50 + 60))"
         it "'$' a couple args in" $
            ("fubar (30 + 50) 4 6 $ someF 3 4", ["fubar", "someF"])
               `correspondsToExpr` "fubar(((30 + 50)), 4, 6, some_f(3, 4))"
         it "lambda + '$'" $
            ("(\\w x y z -> w + x + y + z) 4 5 7 $ 6 * 8", [])
               `correspondsToExpr`
                  "(fun(W, X, Y, Z) -> (((W + X) + Y) + Z) end)(4, 5, 7, (6 * 8))"
         it "$ and infix function" $
            ("foo $ bar `bang` baz", ["foo","bar","baz"])
               `correspondsToExpr` "foo(bang(bar(), baz()))"
         it "'$' with data contructor" $
            ("Foo 5 $ bar [3, 4] 5", ["bar"])
               `correspondsToExpr` "{foo, 5, bar([3, 4], 5)}"
         it "data constructor + '$' II" $
            ("Tower $ fromList [(Start, [1..n]), (End, [])]", ["fromList"])
               `correspondsToExpr`
                  "{tower, from_list([{start, lists:seq(1, N)}, {end, []}])}"
         it "multiple deep '$'s" $
            ("fubar (30 + 50) 4 6 $ someF 3 4 5 $ someF 3 6 4 $ 4 + 5", ["fubar", "someF"])
               `correspondsToExpr`
                  "fubar(((30 + 50)), 4, 6, some_f(3, 4, 5, some_f(3, 6, 4, (4 + 5))))"
      it "enmeration with non-top-level max value" $
         ("[1..n]", []) `correspondsToExpr` "lists:seq(1, N)"
      it "enumeration with toplevel function as max" $
         ("[1..n]", ["n"]) `correspondsToExpr` "lists:seq(1, n())"
      it "take . list comprehension" $
         ("take 20 $ [ x * 2 | x <- [0..num], (x `rem` 3) /= 2 ]", ["take"])
            `correspondsToExpr`
               "take(20, [ (X * 2) || X <- lists:seq(0, Num), (((X rem 3)) /= 2) ])"
      it "lambda" $
         ("(\\x y -> x + y) 4 5", [])
            `correspondsToExpr` "(fun(X, Y) -> (X + Y) end)(4, 5)"
      it "higher-order lambda" $
         ("foldl (\\x y -> x + y) 0 [1..10]", ["foldl"]) `correspondsToExpr`
            "lists:foldl((fun(X, Y) -> (X + Y) end), 0, lists:seq(1, 10))"
      it "higher-order named function" $
         ("foldl add 0 [1..10]", ["foldl", "add"], M.fromList [("add",2)])
            `correspondsToExprWArities`
               "lists:foldl(fun add/2, 0, lists:seq(1, 10))"
      it "do" $
         ("do a <- getLine\n" <>
          "   let b = reverse a\n" <>
          "   return (\"-\" ++ b)",  ["getLine", "reverse", "return"])
            `correspondsToExpr`
               ("A = io:get_line(\"\"),\n" <>
                "B = lists:reverse(A),\n" <>
                "((\"-\" ++ B))")
      it "data constructor" $
         ("Foo 5 6", [])
            `correspondsToExpr` "{foo, 5, 6}"
      it "escaped \\" $
         ("a \\\\  [2, 2]", ["a"])
            `correspondsToExpr` "(a() -- [2, 2])"
      it "plain data constructor" $
         ("Foo 4 bar 5", ["bar"])
            `correspondsToExpr` "{foo, 4, bar(), 5}"
      describe "patterns" $ do
         it "list pattern" $
            ("do let [x, 5, z] = map (\\x -> x + a) b\n" <>
             "   return z", ["a", "b", "map", "return"])
               `correspondsToExpr`
                  "[ X, 5, Z ] = lists:map((fun(X) -> (X + a()) end), b()), Z"
         it "list pattern 2" $
            let (ParseOk p) = parsePat "[x, 5, z]"
            in (pat p) `shouldBe` "[ X, 5, Z ]"
      describe "case expressions" $
         it "with inner pattern matching" $
            ("case x of\n" <>
             "   Just (y:_) -> y\n" <>
             "   Nothing    -> 10", ["x"])
                `correspondsToExpr`
                   ("case x() of\n" <>
                    "   {just, [ Y | _ ]} -> Y;\n" <>
                    "   nothing           -> 10\n" <>
                    "end")
      it "list with variables and unit contructors" $
         ("[Foo, bar]", [])
            `correspondsToExpr` "[foo, Bar]"
      it "variable function application" $
         ("a 5", [])
            `correspondsToExpr` "A(5)"
      it "higher-order toplevel function calls" $
         ("map higherOrder [0..5]", ["higherOrder", "map"], M.fromList [("higherOrder", 1)])
            `correspondsToExprWArities`
               "lists:map(fun higher_order/1, lists:seq(0, 5))"
      it "inner function application" $
         ("camelOne $ camelTwo 5", ["camelOne", "camelTwo"])
            `correspondsToExpr` "camel_one(camel_two(5))"
      it "inner infix function application" $
         ("4 == camelF 5", ["camelF"])
            `correspondsToExpr` "(4 == camel_f(5))"
      it "function in data constructor" $
         ("DataConstructor $ someF 5", ["someF"])
            `correspondsToExpr` "{data_constructor, some_f(5)}"
   describe "declarations" $ do
      let correspondsTo hs erl =
             let (ParseOk d) = parseDecl hs
             in (uP . decl undefined $ d) `shouldBe` (uP erl)
          correspondsToWith (hs, topLevels, fArities) erl =
             let (ParseOk d) = parseDecl hs
             in (uP . decl (ParseContext { topLevels=topLevels
                                         , fArities=M.fromList fArities
                                         }) $ d) `shouldBe` (uP erl)
      describe "function declarations" $ do
         it "one clause" $
            ("f x y = (y - 1) * x", [], [])
               `correspondsToWith` "f(X, Y) -> (((Y - 1)) * X)."
         it "multiple clauses" $
            let (ParseOk m) = parseModule $
                  "f (Just _) 0 = 1\nf (Just n0) n1 = n0 / n1\nf Nothing _ = 0"
                e =
                  ("f({just, _}, 0)   -> 1;\n" <>
                   "f({just, N0}, N1) -> (N0 / N1);\n" <>
                   "f(nothing, _)     -> 0.")
            in (uP . unlines . drop 2 . lines . erlangIt $ m) `shouldBe` (uP e)
         it "function with case expression" $
            let (ParseOk m) = parseModule $
                  "f x =\n   case x / 3 of\n      1 -> 99\n      _ -> x"
                e =
                  "f(X) -> case (X / 3) of 1 -> 99; _ -> X end."
            in (uP . unlines . drop 2 . lines . erlangIt $ m) `shouldBe` (uP e)
         it "toplevel with no args" $
            let (ParseOk d) = parseDecl "fN = 5 + 3"
            in (uP . (decl ParseContext{topLevels=[]}) $ d)
                   `shouldBe`
                      "f_n() -> (5 + 3)."
      describe "type declarations" $ do
         it "maybe type" $
            "data Maybe a = Just a | Nothing"
               `correspondsTo` "-type maybe(A) :: {just, A} | nothing."
         it "void" $
            "data Void = Void" `correspondsTo` "-type void() :: void."
         it "product type" $
            "data Foo = Foo Int String Bool" `correspondsTo`
               "-type foo() :: {foo, integer(), string(), boolean()}."
         it "sum type" $
            "data Weekend = Saturday | Sunday"
               `correspondsTo` "-type weekend() :: saturday | sunday."
         it "native types" $
            "data N = N Int Integer String Char Bool [Float] Double"
               `correspondsTo`
                  ("-type n() :: {n, integer(), integer(), string(), char(),"
                  <> " boolean(), [float()], float()}.")
         it "list of functions" $
            "data Foo a b c = Bar [(a -> b) -> (b -> c) -> (a -> c)]"
               `correspondsTo`
                  ("-type foo(A, B, C) :: {bar, " <>
                  "[fun((fun((A) -> B), fun((B) -> C)) -> fun((A) -> C))]}.")
         it "product with tuple" $
            "data A t = A (String, Int) [(Int, t)]" `correspondsTo`
               "-type a(T) :: {a, {string(), integer()}, [{integer(), T}]}."
         it "complicated higher-order type declaration" $
            ("data T a b c = " <>
            "T a (a -> (a -> (b -> c) -> b) -> c -> (c -> a -> (b -> a)))")
              `correspondsTo`
               ("-type t(A, B, C) :: " <>
                "{t, A, fun((A, fun((A, fun((B) -> C)) -> B), C) ->" <>
                          " fun((C, A) -> fun((B) -> A)))}.")
         it "snake-cases" $
            ("data CamelThing oT = CamelThing oT\n" <>
             "                   | OtherThing")
                `correspondsTo`
                   ("-type camel_thing(OT) :: {camel_thing, OT}\n" <>
                    "                       | other_thing.")

   describe "patterns" $ do
      let correspondsToPat hs erl =
            let (ParseOk p) = parsePat hs
            in ((uP . pat) p)
                  `shouldBe` (uP erl)
      it "recognizes algebraic unit" $
         "UnitType" `correspondsToPat` "unit_type"
      it "recognizes algebraic type with arguments" $
         "OtherType 25 True" `correspondsToPat` "{other_type, 25, true}"
   describe "modules" $ do
      -- drops module and export declarations:
      let correspondsToModule hs erl =
            let (ParseOk m) = parseModule hs
            in (uP . unlines . drop 2 . lines $ erlangIt m)
                  `shouldBe` (uP erl)
      it "standard pattern-matching function definitions" $
         ("f (Fu x) = x + 5\n" <>
          "f Bar    = 7")
             `correspondsToModule`
                ("f({fu, X}) -> (X + 5);\n" <>
                 "f(bar) -> 7.")
      it "module w/ type declaration + explicit toplevel type sig" $
         ("data Maybe a = Just a | Nothing\n\n"<>
          "f :: Maybe Int -> Int\n" <>
          "f (Just i) = i + 5\n" <>
          "f Nothing = 5")
            `correspondsToModule`
               ("-type maybe(A) :: {just, A} | nothing.\n" <>
               "-spec f(maybe(integer())) -> integer().\n" <>
               "f({just, I}) -> (I + 5);\n" <>
               "f(nothing) -> 5.")
      it "if expression and simple type signature" $
         ("bouncer :: Int -> String\n" <>
          "bouncer age =\n" <>
          "   if age >= 21\n" <>
          "      then \"Welcome!\"\n" <>
          "      else \"You're too young to drink here, buddy.\"")
             `correspondsToModule`
                ("-spec bouncer(integer()) -> string().\n" <>
                 "bouncer(Age) ->\n" <>
                 "   case (Age >= 21) of\n" <>
                 "      true  -> \"Welcome!\";\n" <>
                 "      false -> \"You're too young to drink here, buddy.\"\n" <>
                 "   end.")
      it "4-element type sig" $
         ("slope :: Float -> Float -> Float -> Float -> Float\n" <>
          "slope x1 y1 x2 y2 =\n" <>
          "   (y2 - y1) / (x2 - x1)")
           `correspondsToModule`
            ("-spec slope(float(), float(), float(), float()) -> float()."
             <> "\n" <>
             "slope(X1, Y1, X2, Y2) ->" <>
             "   (((Y2 - Y1)) / ((X2 - X1))).")
      it "pattern-matching function args" $
         pending
      it "multiple type vars" $
         ("data Foo a b c = Foo a b c\n\n"<>
          "f :: Foo Int String Int -> Foo String Int Int\n" <>
          "f (Foo a b c) = Foo b a c")
             `correspondsToModule`
                ("-type foo(A, B, C) :: {foo, A, B, C}.\n\n" <>
                 "-spec f(foo(integer(), string(), integer())) ->"
                 <> " foo(string(), integer(), integer()).\n" <>
                 "f({foo, A, B, C}) -> {foo, B, A, C}.")
      it "definition and calling names should match" $
         ("thingOne noun = \"the \" ++ noun\n" <>
          "thingTwo noun0 noun1 = noun0 ++ \" in \" ++ (thingOne noun1)\n" <>
          "seuss = thingTwo \"cat\" \"hat\"")
             `correspondsToModule`
                ("thing_one(Noun) -> (\"the \" ++ Noun).\n" <>
                 "thing_two(Noun0, Noun1) -> (Noun0 ++ (\" in \" ++ (thing_one(Noun1)))).\n" <>
                 "seuss() -> thing_two(\"cat\", \"hat\").")
      it "definition and type should match" $
         ("camelIsh :: Int -> String\n" <>
          "camelIsh a = 4")
             `correspondsToModule`
                ("-spec camel_ish(integer()) -> string().\n" <>
                 "camel_ish(A) -> 4.")
      it "exports and definitions match" $
         let (ParseOk m) =
                parseModule $
                   ("module ThingHere (fName) where\n" <>
                    "fName x = x + 50")
         in (uP . erlangIt $ m) `shouldBe` (uP $
               ("-export([f_name/1]).\n" <>
                "f_name(X) -> (X + 50)."))

   describe "export declarations" $ do
      let noTop = "foo = [3, 4, 5]\nbar y = map (\\x -> x + y) foo"
      it "export all" $
         let ex = noTop
             exportLine = ((head . lines) $ main'' ex)
         in exportLine `shouldBe` "-export([bar/1, foo/0])."
      it "export some" $
         let ex = "module Foo (bar, baz) where\n" <> noTop <> "\nbaz = 5"
             exportLine = ((head . lines) $ main'' ex)
         in exportLine `shouldBe` "-export([bar/1, baz/0])."
      it "exporting data types" $ pending
      it "export all II" $
         let ex = "f = 5\ng x = 10 + x\nh x y = x + y"
             exportLine = ((head . lines) $ main'' ex) -- DRY w above
         in exportLine `shouldBe` "-export([f/0, g/1, h/2])."
   describe "compile code with erlc" $ do
      it "pipelines through" $ do
         let exx = "import Language.Spaceship\nmain = putStrLn \"hi\""
         writeFile "test_file.erl" $
            "-module(test_file).\n" <> ((unlines . tail . lines) $ main'' exx)
         result <- system "erlc test_file.erl"
         removeFile "test_file.erl"
         removeFile "test_file.beam"
         result `shouldBe` ExitSuccess
   describe "name format conversions" $ do
      it "basics" $
         camelToSnake "oneThingHere" `shouldBe` "one_thing_here"
      it "camel <-> snake should be a bijection" $ do
            property $
               \(NonEmpty (n:ame)) -> n /= '_' ==>
                  let name = (toUpper n):ame
                  in (snakeToCamel . camelToSnake $ name) == name

-- uglyPrint:
-- (Erlang is whitespace-insensitive)
uP cs = uP' [ if c `elem` "\n\r\t" then ' ' else c | c <- cs ]
   where
      uP' (' ':' ':xs) = uP (' ':xs)
      uP' (' ':[]) = ""
      uP' (x:xs) = x:uP xs
      uP' [] = []

