module MonadLib (Monad, Option, Id, Either, Reader, Writer, State) where

import Prelude ((++), (.), ($), const, id, Monoid, mempty, (<>))
import Data.Tuple (swap)
import Data.Function ((&))

-- Helpers
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f = swap . mapFst f . swap

-- Monad definition
class Monad m where
  unit :: a -> m a
  flatten :: m (m a) -> m a
  map :: m a -> (a -> b) -> m b

  flatMap :: m a -> (a -> m b) -> m b
  flatMap x f = flatten (map x f)

-- List monad
instance Monad [] where
  unit a = [a]

  flatten [] = []
  flatten (h:t) = h ++ (flatten t)

  map [] _ = []
  map (h:t) f = (f h):(map t f)

-- Option monad
data Option a = Some a | None

instance Monad Option where
  unit = Some

  flatten None = None
  flatten (Some x) = x

  map None _ = None
  map (Some x) f = Some . f $ x

-- Identity monad
data Id a = Id a

instance Monad Id where
  unit = Id

  flatten (Id x) = x

  map (Id x) f = Id . f $ x

-- Either monad
data Either a b = Left a | Right b

instance Monad (Either a) where
  unit = Right

  flatten (Left l) = Left l
  flatten (Right (Left l)) = Left l
  flatten (Right (Right r)) = Right r

  map (Left l) _ = Left l
  map (Right r) f = Right . f $ r

-- Reader monad
data Reader env a = Reader { runReader :: env -> a }

ask :: Reader env env
ask = Reader id

instance Monad (Reader env) where
  unit = Reader . const

  flatten r = Reader (\e -> runReader (runReader r e) e)

  map r f = Reader (f . runReader r)

-- Writer monad
data Writer log a = Writer a log

instance (Monoid log) => Monad (Writer log) where
  unit x = Writer x mempty

  flatten (Writer (Writer x l1) l2) = Writer x (l1 <> l2)

  map (Writer x l) f = Writer (f x) l

-- State monad
data State state a = State { runComp :: state -> (state, a) }

instance Monad (State state) where
  unit x = State (\s -> (s, x))

  flatten comp1 = State (\s1 ->
    let (s2, comp2) = runComp comp1 s1 in
      runComp comp2 s2)

  map comp f = State (mapSnd f . runComp comp)

-- Continuation monad
data Continuation r a = Continuation { runCont :: (a -> r) -> r }

instance Monad (Continuation r) where
  unit = Continuation . (&)

  flatten  = _

  map (Continuation runner) f = Continuation (\cont -> runner (cont . f))
