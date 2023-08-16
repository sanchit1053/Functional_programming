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

class Graph gr where
  {-# MINIMAL empty, isEmpty, match, mkGraph, labNodes #-}

  -- | An empty 'Graph'.
  empty     :: gr a b

  -- | True if the given 'Graph' is empty.
  isEmpty   :: gr a b -> Bool

  -- | Decompose a 'Graph' into the 'MContext' found for the given node and the
  -- remaining 'Graph'.
  match     :: Node -> gr a b -> Decomp gr a b

  -- | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
  --
  --   For graphs that are also instances of 'DynGraph', @mkGraph ns
  --   es@ should be equivalent to @('insEdges' es . 'insNodes' ns)
  --   'empty'@.
  mkGraph   :: [LNode a] -> [LEdge b] -> gr a b

  -- | A list of all 'LNode's in the 'Graph'.
  labNodes  :: gr a b -> [LNode a]

class (Graph gr) => DynGraph gr where
  -- | Merge the 'Context' into the 'DynGraph'.
  --
  --   Context adjacencies should only refer to either a Node already
  --   in a graph or the node in the Context itself (for loops).
  --
  --   Behaviour is undefined if the specified 'Node' already exists
  --   in the graph.
  (&) :: Context a b -> gr a b -> gr a b

newtype Gr a b = Gr (Map Node (Context' a b))
type Context' a b = (Adj b, a , Adj b)


instance Graph Gr where
    empty = Gr M.empty
    isEmpty (Gr g) = M.null g
    match v (Gr g) = maybe (Nothing, g) ( first Just. uncurry (cleanSplit v)) . fun $ M.updateLookupWithKey (const (const Nothing )) v g
                where fun (m, g') = fmap (flip (,) g') m


cleanSplit v (p, l, s) g = (c, Graph g')
    where c = (p', v , l, s) 
          p' = rmLoops p 
          s' = rmLoops s
          rmLoops = filter ((/=v) snd)
          g' = updAdj s' (clearPred v). updAdj p' (clearSucc v) $ g

first f = gmap (\(p,v,l,s)->(p,v,f l,s))

addSucc v l (p, l', s) = (p, l', (l,v):s)
addPred v l (p, l', s) = ((l,v):s, l', s)
clearSucc v _ (p, l, s) = (p, l , filter ((/=v).snd ) s)
clearPred v _ (p, l , s) = (filter ((/=v).snd) p, l, s)
updAdj adj f g = foldl' (\g' (l,v) -> M.adjust (f l) v g') g adj