{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module AlgebraicGraphs where

-- functional pearl : Algebraic Graphs with Class
-- unlabeled: 
-- https://hackage.haskell.org/package/algebraic-graphs
-- labeled: 
-- https://hackage.haskell.org/package/algebraic-graphs-0.5/docs/Algebra-Graph-Labelled.html
-- in agda:
-- https://github.com/algebraic-graphs/agda
-- in scala:
-- https://github.com/algebraic-graphs/scala

data Graph a =  Empty
              | Vertex a
              | Overlay (Graph a) (Graph a)
              | Connect (Graph a) (Graph a)
              deriving Show


class GraphC g where
  type VertexT g
  empty :: g
  vertex :: VertexT g -> g
  overlay :: g -> g -> g
  connect :: g -> g -> g


instance GraphC (Graph a) where
    type VertexT (Graph a) = a
    empty   = Empty
    vertex  = Vertex
    overlay = Overlay
    connect = Connect


edge :: GraphC g => VertexT g -> VertexT g -> g
edge x y = connect (vertex x) (vertex y)

vertices :: GraphC g => [VertexT g] -> g
vertices = foldr overlay empty . map vertex

vertices1und2 :: Graph Int
vertices1und2 = vertices [1 ,2]

