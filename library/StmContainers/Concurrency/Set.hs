module StmContainers.Concurrency.Set
  ( Set,
    new,
    newIO,
    null,
    size,
    focus,
    lookup,
    insert,
    delete,
    reset,
    unfoldlM,
    listT,
  )
where

import qualified Focus as B
import StmContainers.Concurrency.Prelude hiding (delete, empty, foldM, insert, lookup, null, toList)
import qualified StmHamt.Concurrency.SizedHamt as A

-- |
-- A hash set, based on an STM-specialized hash array mapped trie.
newtype Set stm item
  = Set (A.SizedHamt stm item)
  deriving (Typeable)

-- |
-- Construct a new set.
{-# INLINEABLE new #-}
new :: MonadSTM stm => stm (Set stm item)
new =
  Set <$> A.new

-- |
-- Construct a new set in IO.
--
-- This is useful for creating it on a top-level using 'unsafePerformIO',
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINEABLE newIO #-}
newIO :: MonadConc m => m (Set (STM m) item)
newIO =
  Set <$> A.newIO

-- |
-- Check, whether the set is empty.
{-# INLINEABLE null #-}
null :: MonadSTM stm => Set stm item -> stm Bool
null (Set hamt) =
  A.null hamt

-- |
-- Get the number of elements.
{-# INLINEABLE size #-}
size :: MonadSTM stm => Set stm item -> stm Int
size (Set hamt) =
  A.size hamt

-- |
-- Focus on an element with a strategy.
--
-- This function allows to perform simultaneous lookup and modification.
--
-- The strategy is over a unit since we already know,
-- which element we're focusing on and it doesn't make sense to replace it,
-- however we still can decide wether to keep or remove it.
{-# INLINEABLE focus #-}
focus :: (MonadSTM stm, Hashable item) => B.Focus () stm result -> item -> Set stm item -> stm result
focus unitFocus item (Set hamt) =
  A.focus rowFocus id item hamt
  where
    rowFocus =
      B.mappingInput (const item) (const ()) unitFocus

-- |
-- Lookup an element.
{-# INLINEABLE lookup #-}
lookup :: (MonadSTM stm, Hashable item) => item -> Set stm item -> stm Bool
lookup =
  focus (fmap isJust B.lookup)

-- |
-- Insert a new element.
{-# INLINEABLE insert #-}
insert :: (MonadSTM stm, Hashable item) => item -> Set stm item -> stm ()
insert item (Set hamt) =
  A.insert id item hamt

-- |
-- Delete an element.
{-# INLINEABLE delete #-}
delete :: (MonadSTM stm, Hashable item) => item -> Set stm item -> stm ()
delete item (Set hamt) =
  A.focus B.delete id item hamt

-- |
-- Delete all the elements.
{-# INLINEABLE reset #-}
reset :: MonadSTM stm => Set stm item -> stm ()
reset (Set hamt) =
  A.reset hamt

-- |
-- Stream the elements actively.
--
-- Amongst other features this function provides an interface to folding.
{-# INLINEABLE unfoldlM #-}
unfoldlM :: MonadSTM stm => Set stm item -> UnfoldlM stm item
unfoldlM (Set hamt) =
  A.unfoldlM hamt

-- |
-- Stream the elements passively.
{-# INLINE listT #-}
listT :: Set (STM IO) item -> ListT (STM IO) item
listT (Set hamt) =
  A.listT hamt
