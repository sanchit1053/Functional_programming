
{-# LANGUAGE BlockArguments #-}
import Data.List
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as M  



-- Structure of THe tree
type Node = Int
type Adj b = [(b, Node)]
-- Edges directed to the node , Node Number, Node Name or weight , Edges Coming out of the node 
type Context a b = (Adj b, Node, a, Adj b)

type LNode a = (Node, a)
type LEdge a = (Node , Node , a)


type MContext a b = Maybe (Context a b)
type Decomp g a b = (MContext a b, g a b)

-- Tree can be either empty or It can be a list of Contexts 
-- Using the infix operator that is required to implement the one
-- that is presented in the paper 
data Graph a b = Empty | (Context a b) :& Graph a b
            deriving (Eq, Show)

type Graph2 a b =  ([LNode a], [LEdge b])
        -- deriving (Show)

type Context' a b = (Adj b, a, Adj b) 
-- example of graphs



-- Empty graphs
example1 = Empty

-- one node 
example2 = (([ ], 3, 'c', []) :&
           Empty )

-- two nodes
example3 = (([ ], 2, 'b', [("down", 3)]) :&
           (([ ], 3, 'c', [ ]) :&
           Empty ))

-- three nodes example given in paper
{-

    1 <-> 2
    ^   /
    |  L
    3

-}



example4 =  ([("left ", 2), ("up", 3)], 1, 'a', [("right", 2)]) :&
           (([ ], 2, 'b', [("down", 3)]) :&
           (([ ], 3, 'c', [ ]) :&
           Empty ))

-- four nodes Example
{-

    1 <-> 2 -> 4
    ^   /
    |  L
    3

-}
example5 =  ([("left ", 2), ("up", 3)], 1, 'a', [("right", 2)]) :&
           (([ ], 2, 'b' , [("down", 3), ("Sanchit", 4)]) :&
           (([ ], 4, 's' , [] ) :&
           (([ ], 3, 'c' , [ ]) :&
           Empty )))





{-

    1 -> 2 -> 4
    |   
    V  
    3

-}

example6 = ([], 1, 'a', [("first", 2), ("second", 3)]) :&
          (([], 2, 'b', [("third", 4)]) :&
          (([], 3, 'c', []) :&
          (([], 4, 'd', []) :&
          Empty )))



-- return true if empty graph else false
isEmpty :: Graph a b -> Bool
isEmpty Empty = True
isEmpty _  = False

-- maps a function to each context as graphs is a list of contexts 
gmap :: (Context a1 b1 -> Context a2 b2) -> Graph a1 b1 -> Graph a2 b2
gmap f Empty = Empty
gmap f (c :& g) = f c :& gmap f g



-- Reverse the direction of each edge
grev :: Graph a b -> Graph a b
grev = gmap swap
    where   swap (p, v, l, s) = (s, v, l, p)

-- grev example = ([("right",2)],1,'a',[("left ",2),("up",3)]) :& (([("down",3)],2,'b',[]) :& (([],3,'c',[]) :& Empty))


-- Again graph is a list of context so the fold function
ufold :: (Context a b -> t -> t) -> t -> Graph a b -> t
ufold f id Empty = id
ufold f id (c :& g ) = f c (ufold f id g )



-- returns the list of nodes
nodes :: Graph a b -> [Node]
nodes = ufold (\(p, v , l , s ) -> (v:)) [ ]
-- f = \(context) -> g
-- where g is a function that takes list of nodes and adds v
-- id is empty list returns the list of numbers

-- nodes example = [1,2,3]

-- adds both the incoming and outgoing edges to to each group so that there is
-- one edge in each direction for both nodes
undir :: (Eq b) => Graph a b -> Graph a b
undir = gmap (\(p,v, l, s) -> let ps = nub (p++s) in (ps, v, l, ps ))


-- finds the context with the node required and outputs the context and the remaining graph (only the remaining contexts)
-- match:: Node -> Graph a b -> (Maybe (Context a b), Graph a b)

-- match n Empty = (Nothing, Empty)
-- match n ((p, v, l, s) :& g) | v==n = (Just (p,v,l,s), g) -- If node found then return that
--                             | otherwise = addN (p,v,l,s) (match n g) -- otherwise search the remaining tree
--                                 where addN c (c2, g) = (c2, c :& g) -- add the context to the graph





match:: Node -> Graph a b -> (Maybe (Context a b), Graph a b)

match n Empty = (Nothing, Empty)
match n g 
    | isThere n g = (Just (graphContext n (graph12 g)), remNode n g)
    | otherwise = (Nothing, Empty)



isThere n Empty = False
isThere n ((p, v, l, s) :& g) 
    | v == n = True
    | otherwise = isThere n g



gsuc v g = case match v g of
            (Just (_,_,_,s),g) -> map snd s

suc (_, _, _, s) = map snd s
pre (p, _, _, _) = map snd p

dfs [] g = []
dfs (v:vs) (g) = case match v g of
            (Just c, g) -> v : dfs (suc c ++ vs ) g
            (Nothing, g) -> dfs vs g


bfs [] g = []
bfs (v:vs) (g) = case match v g of
            (Just c, g) -> v : bfs (vs ++ suc c ) g
            (Nothing, g) -> bfs vs g


lnodes = ufold (\(p, v , l , s ) -> ((v,l):)) [ ]

ledges g = concat (ufold (joi) [] g) 
    where
        joi (p,v,l,s) = (((foldr (pre v) [] p ) ++ (foldr (nex v) [] s)) :)
        pre v (l', v') = ((v, v',l'):)
        nex v (l' ,v') = ((v',v,l') :)

graph12 :: Graph a b -> Graph2 a b
graph12 g = (lnodes g, ledges g)

-- graph21 :: Graph2 a b -> Graph a b
graph21 g@(lnodes, ledges) =  trim (foldr (expand ledges) Empty lnodes )   

graphContext n g@(lnodes, ledges) = myFirst (myfilter ( \(p,v,l,s) -> (v==n) ) 
                                            (foldr (expand ledges) Empty lnodes))

myFirst (c :& g) = c


expand :: [LEdge a] -> LNode b -> Graph b a -> Graph b a
expand ledges lnode@(a, b) =  ((toAdj1 lnode ledges , a , b , toAdj2 lnode ledges) :&)



toAdj2 :: LNode a -> [LEdge b] -> Adj b
toAdj2 lnode@(n, ln) ledges = map f (filter (g n) ledges)
                    where g n ledge@(a, b, l) 
                            | b == n = True
                            | otherwise = False
                          f ledge@(a, b, l) = (l, a)

                          
toAdj1 :: LNode a -> [LEdge b] -> Adj b
toAdj1 lnode@(n, ln) ledges = map f (filter (g n) ledges)
                    where g n ledge@(a, b, l) 
                            | a == n = True
                            | otherwise = False
                          f ledge@(a, b, l) = (l, b)




trim Empty = Empty 
trim (c@(p, v, l, s) :& g) = c :& (myrem v (trim g))
myrem v g = gmap (rem' v) g 

rem' v' (p, v, l, s) = (p', v, l, s') 
 where 
    p' = filter (noOccur v') p
    s' = filter (noOccur v') s

noOccur v (l, n)
  | v == n = False
  | otherwise = True


 

remNode v g =  gmap (rem' v) (myfilter (\(p,v',l,s)-> (v /= v')) g)
    
myfilter f Empty = Empty
myfilter f (c :& g)
        | f c = c :& myfilter f g
        | otherwise = myfilter f g


remNode' :: Node -> Graph2 a b -> Graph2 a b
remNode' v g@(n, e) = (filter (\(n',l')-> (n'/= v)) n , filter (\(n1, n2, l) -> (n1/=v && n2/= v)) e)


type Path = [Node]
type RTree = [Path]



-- Function that prints the bfs path to all the nodes in the graph 
bft v = bfsPath [[v]]

bfsPath :: [Path] -> Graph a b -> RTree
bfsPath [] g = []
bfsPath (p@(v:_):ps) g = case match v g of
                            (Just c , g') -> p: bfsPath (ps ++ map (:p) (suc c)) g
                            (Nothing, g') -> bfsPath ps g


first p = head. filter p

esp :: Node -> Node -> Graph a b -> [Node] 
esp s t = reverse . first (\(v:_)-> v== t). bft s

type LPath a = [LNode a]
type LRTree a = [LPath a]

instance Eq a => Eq (LPath a) where 
    ((_,x):_) == ((_,y):_) = x == y
instance Ord a => Ord (LPath a) where
    ((_,x):_) < ((_,y):_) = x < y

getPath v = reverse. map fst . first (\((w,_):_) -> w==v)



deg v g = case match v g of
            (Just (p,c',l,s), g') -> length p + length s
            _ -> 0

del v g = case match v g of 
            (Just (p, c', l, s), g') -> g'
            _ -> g


indep Empty = []
indep g = if  length i1 > length i2 then i1 else i2
            where 
                vs = nodes g
                m = maximum (map (flip deg g) vs )
                v = first (\v -> deg v g == m) vs
                (Just c, g') = match v g
                i1 = indep g'
                i2 = v: indep(foldr del g'(pre c ++ suc c)) 