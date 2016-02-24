{-# OPTIONS -Wall #-}


-- | This module defines the Charted chart parser.

module Charted ( Rule
               , Grammar
               , Lexer
               , nextLabel
               , nextData
               , parse
               ) where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M







-- | A @Chart d l@ is essentially a functional directed graph, where the
-- each value of @Chart d l@ is a node, and the set of edges is therefore the
-- edges pointing from that node to some other nodes. 
-- 
-- We represent this as a map from labels @l@ to lists of @Edge d l@ that are
-- labeled with the key. We use a map instead of a list because we want to be
-- able to inspect the labels independently of their values, to make decisions
-- based solely on the label. Since many edges may have the same label, this
-- lets us decide to use (or not) a whole bunch of edges with just a single
-- label inspection. If we had used a list, we'd have as many labels as edges,
-- with many redundancies. The map lets us factor out the common label.

newtype Chart d l = Chart { edges :: M.Map l [Edge d l] }
  deriving (Show,Eq)


emptyChart :: Chart d l
emptyChart = Chart M.empty





-- | An @Edge d l@ is just some data @d@ together with a @Chart d l@ that
-- corresponds to the rest of the chart after that edge.

data Edge d l = Edge d (Chart d l)
  deriving (Show,Eq)





-- | We can "cons" some data onto a chart with a label by making a new chart
-- with a single appropriate edge.

consChart :: Ord l => [(d,l)] -> Chart d l -> Chart d l
consChart dls c =
  Chart $ M.fromListWith (++) [ (l, [Edge d c]) | (d,l) <- dls ]





-- | A matcher is a stateful process that works on a chart and produces some
-- non-deterministic result (in a way that will depend on the chart). We don't
-- ever want to explicitly manipulate the chart state. Instead, we want to use
-- special ways of accessing the chart.

type Matcher d l a = StateT (Chart d l) [] a





-- | The first special way to access the chart is to read the data off of the
-- next edges. We do this by walking across each edge at the front of the
-- current chart state, returning the edge data, and making the next chart
-- state the remaining of the graph after that edge.

nextData :: Matcher d l d
nextData =
  StateT $ \c ->
    do (_,es) <- M.toList (edges c)
       Edge d r <- es
       return (d,r)





-- | The second special way to access the chart is to read off the labels of
-- the next edges. We do this by returning each label, and making the new
-- chart state the current chart state restricted to only the edges with the
-- label in question. This has the consequence that @do l <- nextLabel ; M@
-- will run the matcher @M@ on only a subset of the chart. If we combine this
-- with guards, we can get a kind of short-circuited test. For example:
--
-- @
--    do l <- nextLabel
--       guard (l == "Foo")
--       return "Bar"
-- @
--
-- This matcher will match any edge with the label @"Foo"@, and for those
-- edges it will return @"Bar"@, but for any other edge, the guard will fail
-- and the non-deterministic output will be empty (ie the @StateT@ returns
-- the empty list @[]@ for its values and next states). This would therefore
-- act roughly like a grammar rule @Bar -> Foo@, used in the parsing direction
-- of looking for a @Foo@ and turning it into a @Bar@.
--
-- We can also of course rely on the monad's @fail@ method to avoid using
-- explicit guards, like so:
--
-- @
--    do "Foo" <- nextLabel
--       return "Bar"
-- @

nextLabel :: Ord l => Matcher d l l
nextLabel =
  StateT $ \c ->
    do (l,es) <- M.toList (edges c)
       return (l, Chart (M.fromList [(l,es)]))





-- | A rule is just a matcher that returns some data and a label, so that we
-- can construct a new edge.

type Rule d l = Matcher d l (d,l)





-- | We can match a rule on a chart by running that rule on the chart, and
-- building up a new chart with the produced edges at the head of the chart.
-- Note that this new chart does not include any of the old head edges. It's
-- a chart with only the new edges, pointing to whatever remainder necessary.

matchRuleOnChart :: (Eq d, Ord l) => Rule d l -> Chart d l -> Chart d l
matchRuleOnChart r c =
  Chart $ M.fromListWith
            (++)
            [ (l,[Edge d c']) | ((d,l),c') <- runStateT r c ]





-- | A grammar is just a collection of rules.

type Grammar d l = [Rule d l]





-- | We can match a grammar on a chart by applying each rule in the grammar,
-- and then combining the resultant charts, to produce a chart of all the new
-- edges. Again, this has none of the old head edges, only new head edges.

matchGrammarOnChart :: (Eq d, Ord l) => Grammar d l -> Chart d l -> Chart d l
matchGrammarOnChart g c =
  Chart $ M.unionsWith (++) [ edges (matchRuleOnChart r c) | r <- g ]
    




-- Saturation is the repeated application of a partial function to an element
-- until output is found, collecting up all of the results. For example,
-- suppose we have the function @succLE5@ which maps every number less than 5
-- to its successor.
--
-- @
--    succLE5 :: Int -> Maybe Int
--    succLE5 n
--      | n <= 5    = Just (n+1)
--      | otherwise = Nothing
-- @
--
-- If we now call 'saturate' on @0@ with this function, we get
--
-- @
--    saturate 5 succLE5  ==  [6,5,4,3,2,1,0]
-- @
--
-- This function is similar to an unfold.

saturate :: a -> (a -> Maybe a) -> [a]
saturate x f = go x []
  where
    go y ys =
      case f y of
        Nothing -> y:ys
        Just y' -> go y' (y:ys)





-- | Saturating a chart is basically a process of repeatedly matching a
-- grammar on a chart until the result of the match is a chart with no edges,
-- indicating that the match produced nothing new. It helps to imagine this
-- as a series of stages: Suppose we have a starting chart @C@ which has some
-- unspecified edges. If we match a grammar @G@ against that chart, we get
-- some new edges:
--
-- @
--                  .----e1----.
--    C    ===>    |            v
--                 N0 ---e0---> N1
-- @
--
-- If we then match the grammar on that new chart we get some more edges
--
-- @
--                  .----e1----.              .---------e4---------.
--    C    ===>    |            v    ===>    | .-----e3-------.     |
--                 N0 ---e0---> N1           ||                v    v
--                                           N0 ---e2---> N2   N3   N4
-- @
--
-- And so on until no new edges are produced. We can then squash these down
-- into a single chart, which is the original chart with new edges. This is
-- the process by which all of the possible new edges are added to a chart.
-- After saturating a chart, every grammar rule that could apply to the chart
-- has been applied, and the chart has all possible edges.

saturateChart :: (Eq d, Ord l) => Grammar d l -> Chart d l -> Chart d l
saturateChart g c =
  Chart $ M.unionsWith (++)
                       (map edges (saturate c saturationStepper))
  where
    saturationStepper c' =
      let c'' = matchGrammarOnChart g c'
      in if c'' == emptyChart
         then Nothing
         else Just c''





-- | We can extend a chart by adding a new edge with some data and a label.
-- After doing so, we saturate the chart with the provided grammar, to ensure
-- that the new chart has all the new edges that the added edge can induce
-- according to the grammar.

addToChart :: (Eq d, Ord l)
           => Grammar d l -> [(d,l)] -> Chart d l -> Chart d l
addToChart g dls c = saturateChart g (consChart dls c)





-- An edge spans a chart if the chart after it is the empty chart.

isSpan :: Edge d l -> Bool
isSpan (Edge _ (Chart m)) = M.null m





-- | A lexer is just a map from words to possible labeled data.

type Lexer d l = String -> [(d,l)]





-- We can build a chart from some sequence of lists of labeled data by adding
-- the data to the empty chart one piece at a time.

buildChart :: (Eq d, Ord l)
           => Grammar d l
           -> Lexer d l
           -> [String]
           -> Maybe (Chart d l)
buildChart g lx input = go input emptyChart
  where
    go [] acc = Just acc
    go (w:ws) acc = case lx w of
      [] -> Nothing
      dls -> go ws (addToChart g dls acc)





-- | We can get the labeled data for the spanning edges of a chart by
-- converting to a list and filtering appropriately.

spanningEdges :: Chart d l -> [(d,l)]
spanningEdges c = 
  do (l,es) <- M.toList (edges c)
     e@(Edge d _) <- es
     guard (isSpan e)
     return (d,l)





-- To parse, we take an input sequence consisting of labeled data, and
-- incrementally add each piece to the chart. We then filter to keep only the
-- edges that span the whole chart, and convert that into a list of the
-- data and labels from those spanning edges.
--
-- We reverse the input so that we can work efficiently, and so that the
-- parsing rules have a nice presentation order, where the first labels in the
-- matcher are the first in the chart. This corresponds to parsing from right
-- to left.

parse :: (Eq d, Ord l) => Grammar d l -> Lexer d l -> String -> [(d,l)]
parse g lx input =
  case buildChart g lx (reverse (words input)) of
    Nothing -> []
    Just c -> spanningEdges c