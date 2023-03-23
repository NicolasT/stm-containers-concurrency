module StmContainers.Concurrency.Map
  ( Map,
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

import qualified DeferredFolds.UnfoldlM as C
import qualified Focus as B
import StmContainers.Concurrency.Prelude hiding (delete, empty, foldM, insert, lookup, null, toList)
import qualified StmHamt.Concurrency.Hamt as A

-- |
-- Hash-table, based on STM-specialized Hash Array Mapped Trie.
newtype Map stm key value
  = Map (A.Hamt stm (Product2 key value))

-- |
-- Construct a new map.
{-# INLINEABLE new #-}
new :: MonadSTM stm => stm (Map stm key value)
new =
  Map <$> A.new

-- |
-- Construct a new map in IO.
--
-- This is useful for creating it on a top-level using 'unsafePerformIO',
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINEABLE newIO #-}
newIO :: IO (Map STM key value)
newIO =
  Map <$> A.newIO

-- |
-- Check, whether the map is empty.
{-# INLINEABLE null #-}
null :: MonadSTM stm => Map stm key value -> stm Bool
null (Map hamt) =
  A.null hamt

-- |
-- Get the number of elements.
{-# INLINEABLE size #-}
size :: MonadSTM stm => Map stm key value -> stm Int
size =
  C.foldlM' (\x _ -> return (succ x)) 0 . unfoldlM

-- |
-- Focus on a value by the key.
--
-- This function allows to perform composite operations in a single access
-- to the map's row.
-- E.g., you can look up a value and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focus #-}
focus :: (MonadSTM stm, Hashable key) => B.Focus value stm result -> key -> Map stm key value -> stm result
focus valueFocus key (Map hamt) =
  A.focus rowFocus (\(Product2 key _) -> key) key hamt
  where
    rowFocus =
      B.mappingInput (\value -> Product2 key value) (\(Product2 _ value) -> value) valueFocus

-- |
-- Look up an item.
{-# INLINEABLE lookup #-}
lookup :: (MonadSTM stm, Hashable key) => key -> Map stm key value -> stm (Maybe value)
lookup key =
  focus B.lookup key

-- |
-- Insert a value at a key.
{-# INLINE insert #-}
insert :: (MonadSTM stm, Hashable key) => value -> key -> Map stm key value -> stm ()
insert value key (Map hamt) =
  void (A.insert (\(Product2 key _) -> key) (Product2 key value) hamt)

-- |
-- Delete an item by a key.
{-# INLINEABLE delete #-}
delete :: (MonadSTM stm, Hashable key) => key -> Map stm key value -> stm ()
delete key =
  focus B.delete key

-- |
-- Delete all the associations.
{-# INLINEABLE reset #-}
reset :: MonadSTM stm => Map stm key value -> stm ()
reset (Map hamt) =
  A.reset hamt

-- |
-- Stream the associations actively.
--
-- Amongst other features this function provides an interface to folding.
{-# INLINEABLE unfoldlM #-}
unfoldlM :: MonadSTM stm => Map stm key value -> UnfoldlM stm (key, value)
unfoldlM (Map hamt) =
  fmap (\(Product2 k v) -> (k, v)) (A.unfoldlM hamt)

-- |
-- Stream the associations passively.
{-# INLINE listT #-}
listT :: Map STM key value -> ListT STM (key, value)
listT (Map hamt) =
  fmap (\(Product2 k v) -> (k, v)) (A.listT hamt)
