module StmContainers.Concurrency.Multimap
  ( Multimap,
    new,
    newIO,
    null,
    focus,
    lookup,
    lookupByKey,
    insert,
    delete,
    deleteByKey,
    reset,
    unfoldlM,
    unfoldlMKeys,
    unfoldlMByKey,
    listT,
    listTKeys,
    listTByKey,
  )
where

import qualified Focus as C
import qualified StmContainers.Concurrency.Map as A
import StmContainers.Concurrency.Prelude hiding (delete, empty, foldM, insert, lookup, null, toList)
import qualified StmContainers.Concurrency.Set as B

-- |
-- A multimap, based on an STM-specialized hash array mapped trie.
--
-- Basically it's just a wrapper API around @'A.Map' key ('B.Set' value)@.
newtype Multimap stm key value
  = Multimap (A.Map stm key (B.Set stm value))
  deriving (Typeable)

-- |
-- Construct a new multimap.
{-# INLINE new #-}
new :: MonadSTM stm => stm (Multimap stm key value)
new =
  Multimap <$> A.new

-- |
-- Construct a new multimap in IO.
--
-- This is useful for creating it on a top-level using 'unsafePerformIO',
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINE newIO #-}
newIO :: IO (Multimap STM key value)
newIO =
  Multimap <$> A.newIO

-- |
-- Check on being empty.
{-# INLINE null #-}
null :: MonadSTM stm => Multimap stm key value -> stm Bool
null (Multimap map) =
  A.null map

-- |
-- Focus on an item by the value and the key.
--
-- This function allows to perform simultaneous lookup and modification.
--
-- The focus is over a unit since we already know,
-- which value we're focusing on and it doesn't make sense to replace it,
-- however we still can decide wether to keep or remove it.
{-# INLINE focus #-}
focus :: (MonadSTM stm, Hashable key, Hashable value) => C.Focus () stm result -> value -> key -> Multimap stm key value -> stm result
focus unitFocus@(Focus concealUnit _) value key (Multimap map) = A.focus setFocus key map
  where
    setFocus = C.Focus conceal reveal
      where
        conceal = do
          (output, change) <- concealUnit
          case change of
            C.Set () ->
              do
                set <- B.new
                B.insert value set
                return (output, C.Set set)
            _ ->
              return (output, C.Leave)
    reveal set = do
      output <- B.focus unitFocus value set
      change <- bool C.Leave C.Remove <$> B.null set
      return (output, change)

-- |
-- Look up an item by a value and a key.
{-# INLINE lookup #-}
lookup :: (MonadSTM stm, Hashable key, Hashable value) => value -> key -> Multimap stm key value -> stm Bool
lookup value key (Multimap m) =
  maybe (return False) (B.lookup value) =<< A.lookup key m

-- |
-- Look up all values by key.
{-# INLINE lookupByKey #-}
lookupByKey :: (MonadSTM stm, Hashable key) => key -> Multimap stm key value -> stm (Maybe (B.Set stm value))
lookupByKey key (Multimap m) =
  A.lookup key m

-- |
-- Insert an item.
{-# INLINEABLE insert #-}
insert :: (MonadSTM stm, Hashable key, Hashable value) => value -> key -> Multimap stm key value -> stm ()
insert value key (Multimap map) = A.focus setFocus key map
  where
    setFocus = Focus conceal reveal
      where
        conceal = do
          set <- B.new
          B.insert value set
          return ((), C.Set set)
        reveal set = do
          B.insert value set
          return ((), C.Leave)

-- |
-- Delete an item by a value and a key.
{-# INLINEABLE delete #-}
delete :: (MonadSTM stm, Hashable key, Hashable value) => value -> key -> Multimap stm key value -> stm ()
delete value key (Multimap map) = A.focus setFocus key map
  where
    setFocus = Focus conceal reveal
      where
        conceal = returnChange C.Leave
        reveal set = do
          B.delete value set
          B.null set >>= returnChange . bool C.Leave C.Remove
        returnChange c = return ((), c)

-- |
-- Delete all values associated with the key.
{-# INLINEABLE deleteByKey #-}
deleteByKey :: (MonadSTM stm, Hashable key) => key -> Multimap stm key value -> stm ()
deleteByKey key (Multimap map) =
  A.delete key map

-- |
-- Delete all the associations.
{-# INLINE reset #-}
reset :: MonadSTM stm => Multimap stm key value -> stm ()
reset (Multimap map) =
  A.reset map

-- |
-- Stream associations actively.
--
-- Amongst other features this function provides an interface to folding.
unfoldlM :: MonadSTM stm => Multimap stm key value -> UnfoldlM stm (key, value)
unfoldlM (Multimap m) =
  A.unfoldlM m >>= \(key, s) -> (key,) <$> B.unfoldlM s

-- |
-- Stream keys actively.
unfoldlMKeys :: MonadSTM stm => Multimap stm key value -> UnfoldlM stm key
unfoldlMKeys (Multimap m) =
  fmap fst (A.unfoldlM m)

-- |
-- Stream values by a key actively.
unfoldlMByKey :: (MonadSTM stm, Hashable key) => key -> Multimap stm key value -> UnfoldlM stm value
unfoldlMByKey key (Multimap m) =
  lift (A.lookup key m) >>= maybe mempty B.unfoldlM

-- |
-- Stream associations passively.
listT :: Multimap STM key value -> ListT STM (key, value)
listT (Multimap m) =
  A.listT m >>= \(key, s) -> (key,) <$> B.listT s

-- |
-- Stream keys passively.
listTKeys :: Multimap STM key value -> ListT STM key
listTKeys (Multimap m) =
  fmap fst (A.listT m)

-- |
-- Stream values by a key passively.
listTByKey :: (Hashable key) => key -> Multimap STM key value -> ListT STM value
listTByKey key (Multimap m) =
  lift (A.lookup key m) >>= maybe mempty B.listT
