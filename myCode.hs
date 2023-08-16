-- module Main where

-- import Data.Graph.Inductive
-- import Data.Graph.Inductive.Example

-- main :: IO ()
-- main = return ()



{-# LANGUAGE BlockArguments #-}
import Data.List
import Debug.Trace
import Data.GraphViz
import Data.Map (Map)
import qualified Data.Map as M  


-- Structure of THe tree
type Node = Int
type Adj b = [(b, Node)]
type Context a b = (Adj b, Node, a, Adj b)
type LNode a = (Node, a)
type LEdge a = (Node , Node , a)

type MContext a b = Maybe (Context a b)
type Decomp g a b = (MContext a b, g a b)

-- Tree can be either empty or It can be a list of Contexts 
-- Using the infix operator that is required to implement the one
-- that is presented in the paper 
-- data Graph a b = Empty | (Context a b) :& Graph a b
--             deriving (Eq, Show)

newtype Gr a b  = Gr (GraphRep a b)
    deriving (Show)
type GraphRep a b = Map Node (Context' a b)

type Context' a b = (Adj b, a, Adj b) 
-- example of graphs

empty = Gr M.empty
isEmpty (Gr g) = M.null g


-- match     :: Node -> Gr a b -> Decomp Gr a b
match v gr@(Gr g) = maybe (Nothing, gr) 
                    (first Just .uncurry (cleanSplit v)) 
                    . (\(m,g') -> fmap (flip (,) g') m)
                      $ M.updateLookupWithKey (const (const Nothing)) v g

matchAny (Gr g)   = maybe (error "Match Exception, Empty Graph")
                            (uncurry (uncurry cleanSplit))
                            (M.minViewWithKey g)



-- -- Empty graphs
-- -- example1 = Empty

-- -- -- one node 
-- -- example2 = (([ ], 3, 'c', [ ]) :&
-- --            Empty )

-- -- -- two nodes
example3 = (([ ], 3, 'b', [("down", 2)]) &
           (([ ], 2, 'c', [ ]) &
           empty ))

-- -- -- three nodes 
-- -- example4 =  ([("left ", 2), ("up", 3)], 1, 'a', [("right", 2)]) :&
-- --            (([ ], 2, 'b', [("down", 3)]) :&
-- --            (([ ], 3, 'c', [ ]) :&
-- --            Empty ))

-- -- -- four nodes Example graph given in paper
-- -- example5 =  ([("left ", 2), ("up", 3)], 1, 'a', [("right", 2)]) :&
-- --            (([ ], 2, 'b' , [("down", 3), ("Sanchit", 4)]) :&
-- --            (([ ], 4, 's' , [] ) :&
-- --            (([ ], 3, 'c' , [ ]) :&
-- --            Empty )))



-- -- example6 = ([], 1, 'a', [("first", 2), ("second", 3)]) :&
-- --           (([], 2, 'b', [("third", 4)]) :&
-- --           (([], 3, 'c', []) :&
-- --           (([], 4, 'd', []) :&
-- --           Empty )))



-- -- return true if empty graph else false
-- isEmpty :: Graph a b -> Bool
-- isEmpty Empty = True
-- isEmpty _  = False

-- -- maps a function to each context as graphs is a list of contexts 
-- gmap :: (Context a1 b1 -> Context a2 b2) -> Graph a1 b1 -> Graph a2 b2
-- gmap f Empty = Empty
-- gmap f (c :& g) = f c :& gmap f g



-- -- Reverse the direction of each edge
-- grev :: Graph a b -> Graph a b
-- grev = gmap swap
--     where   swap (p, v, l, s) = (s, v, l, p)

-- -- grev example = ([("right",2)],1,'a',[("left ",2),("up",3)]) :& (([("down",3)],2,'b',[]) :& (([],3,'c',[]) :& Empty))


-- -- Again graph is a list of context so the fold function
-- ufold :: (Context a b -> t -> t) -> t -> Graph a b -> t
-- ufold f id Empty = id
-- ufold f id (c :& g ) = f c (ufold f id g )



-- -- returns the list of nodes
-- nodes :: Graph a b -> [Node]
-- nodes = ufold (\(p, v , l , s ) -> (v:)) [ ]
-- -- f = \(context) -> g
-- -- where g is a function that takes list of nodes and adds v
-- -- id is empty list returns the list of numbers

-- -- nodes example = [1,2,3]

-- -- adds both the incoming and outgoing edges to to each group so that there is
-- -- one edge in each direction for both nodes
-- undir :: (Eq b) => Graph a b -> Graph a b
-- undir = gmap (\(p,v, l, s) -> let ps = nub (p++s) in (ps, v, l, ps ))


-- -- finds the context with the node required and outputs the context and the remaining graph (only the remaining contexts)
-- match:: Node -> Graph a b -> (Maybe (Context a b), Graph a b)

-- match n Empty = (Nothing, Empty)
-- match n ((p, v, l, s) :& g) | v==n = (Just (p,v,l,s), g) -- If node found then return that
--                             | otherwise = addN (p,v,l,s) (match n g) -- otherwise search the remaining tree
--                                 where addN c (c2, g) = (c2, c :& g) -- add the context to the graph

-- gsuc v g = case match v g of
--             (Just (_,_,_,s),g) -> map snd s

-- suc (_, _, _, s) = map snd s

-- dfs [] g = []
-- dfs (v:vs) (g) = case match v g of
--             (Just c, g) -> v : dfs (suc c ++ vs ) g
--             (Nothing, g) -> dfs vs g


-- bfs [] g = []
-- bfs (v:vs) (g) = case match v g of
--             (Just c, g) -> v : bfs (vs ++ suc c ) g
--             (Nothing, g) -> bfs vs g

(&) :: Context a b -> Gr a b -> Gr a b
(p,v,l,s) & (Gr g) = Gr . updAdj p (addSucc v) . updAdj s (addPred v) $ M.alter addCntxt v g
    where
      addCntxt = maybe (Just cntxt')
                       (const (error ("Node Exception, Node: "++show v)))
      cntxt' = (p,l,s)


ufold :: (Context a b -> c -> c) -> c -> Gr a b -> c
ufold f u g
  | isEmpty g = u
  | otherwise = f c (ufold f u g')
  where
    (c,g') = matchAny g

gmap :: (Context a b -> Context c d) -> Gr a b -> Gr c d
gmap f = ufold (\c->(f c&)) empty

nmap :: (a -> c) -> Gr a b -> Gr c b
nmap f = gmap (\(p,v,l,s)->(p,v,f l,s))

first = nmap




cleanSplit ::  Node -> Context' a b -> GraphRep a b -> (Context a b, Gr a b)
cleanSplit v (p,l,s) g = traceShow (v) (c, Gr g')
  where
    -- Note: loops are kept only in successor list
    c = (p', v, l, s)
    p' = rmLoops p
    s' = rmLoops s
    rmLoops = filter ((/=v) . snd)
    g' = updAdj s' (clearPred v) . updAdj p' (clearSucc v) $ g


addSucc :: Node -> b -> Context' a b -> Context' a b
addSucc v l (p,l',s) = (p,l',(l,v):s)

addPred :: Node -> b -> Context' a b -> Context' a b
addPred v l (p,l',s) = ((l,v):p,l',s)

clearSucc :: Node -> b -> Context' a b -> Context' a b
clearSucc v _ (p,l,s) = (p,l,filter ((/=v).snd) s)

clearPred :: Node -> b -> Context' a b -> Context' a b
clearPred v _ (p,l,s) = (filter ((/=v).snd) p,l,s)

updAdj :: Adj b -> (b -> Context' a b -> Context' a b) -> GraphRep a b -> GraphRep a b
updAdj adj f g = foldl' (\g' (l,v) -> M.adjust (f l) v g') g adj

