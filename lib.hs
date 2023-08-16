
{-# LANGUAGE BlockArguments #-}
import Data.Graph.Inductive as IN
import Data.Graph.Inductive.Tree as Tr
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Text.Lazy (unpack)
import Debug.Trace (traceShow, traceIO)


example4 :: Tr.Gr Char String 
example4 =  ([("left ", 2), ("up", 3)], 1, 'a', [("right", 2)]) &
           (([ ], 2, 'b', [("down", 3)]) &
           (([ ], 3, 'c', [ ]) &
           IN.empty ))

mysucc (_, _, _, s) = map snd s


mydfs :: [Node] -> Tr.Gr a b -> [Node]
mydfs [] g = []
mydfs (v:vs) (g) = case match v g of
            (Just c, g) -> v : mydfs (mysucc c ++ vs ) g
            (Nothing, g) -> mydfs vs g


test v g = match 1 (snd (match v g)) 

-- f :: Tr.Gr a b -> c -> c
-- f g a = do
--             traceIO $ unpack $ renderDot $ toDot $ graphToDot nonClusteredParams g
--             return a
