module MonadicStyleBacktracking where

import Control.Monad


person :: ( MonadPlus m ) => m String
person = return "abraham" `mplus` return "lot" 
      `mplus` return "milcah" 
      `mplus` return "nachor"
      `mplus` return "sarah" 
      `mplus` return "terach" 
      `mplus` return "yiscah"

child :: ( MonadPlus m ) => String -> m String
child "terach" = return "abraham" `mplus` return "nachor" `mplus` return "haran"
child "abraham" = return "isaac"
child "haran" = return "lot" `mplus` return "milcah" `mplus` return "yiscah"
child "sarah" = return "isaac"
child _ = mzero

listAllP :: (MonadPlus m ) => m String
listAllP = do p <- person ; return p

listAllPC :: (MonadPlus m ) => m (String, String)
listAllPC = do p <- person ; c <- child p ; return ( p , c )

descendant :: ( MonadPlus m ) => String -> m String
descendant = transitiveClosure child

transitiveClosure :: ( MonadPlus m ) => ( a -> m a ) -> ( a -> m a )
transitiveClosure m = m +++ ( transitiveClosure m `at` m )

at :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f `at` g = \ a -> g a >>= f

(+++) :: (MonadPlus m) => (a -> m b) -> (a -> m b) -> (a -> m b)
f +++ g = \ a -> (f a `mplus` g a)

transitiveClosure' :: (MonadPlus m) => (a -> m a) -> (a -> m a)
transitiveClosure' m = ( return +++ transitiveClosure' m ) `at` m

-- query1 = run ( descendant "terach" :: BacktrM String ) :: [] String