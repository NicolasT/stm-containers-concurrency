module StmContainers.Concurrency.Bimap
  ( Bimap,
    new,
    newIO,
    null,
    size,
    focusLeft,
    focusRight,
    lookupLeft,
    lookupRight,
    insertLeft,
    insertRight,
    deleteLeft,
    deleteRight,
    reset,
    unfoldlM,
    listT,
  )
where

import qualified Focus as B
import qualified StmContainers.Concurrency.Map as A
import StmContainers.Concurrency.Prelude hiding (delete, empty, foldM, insert, lookup, null, toList)

-- |
-- Bidirectional map.
-- Essentially, a bijection between subsets of its two argument types.
--
-- For one value of the left-hand type this map contains one value
-- of the right-hand type and vice versa.
data Bimap stm leftKey rightKey
  = Bimap !(A.Map stm leftKey rightKey) !(A.Map stm rightKey leftKey)
  deriving (Typeable)

-- |
-- Construct a new bimap.
{-# INLINE new #-}
new :: MonadSTM stm => stm (Bimap stm leftKey rightKey)
new =
  Bimap <$> A.new <*> A.new

-- |
-- Construct a new bimap in IO.
--
-- This is useful for creating it on a top-level using 'unsafePerformIO',
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINE newIO #-}
newIO :: IO (Bimap STM leftKey rightKey)
newIO =
  Bimap <$> A.newIO <*> A.newIO

-- |
-- Check on being empty.
{-# INLINE null #-}
null :: MonadSTM stm => Bimap stm leftKey rightKey -> stm Bool
null (Bimap leftMap _) =
  A.null leftMap

-- |
-- Get the number of elements.
{-# INLINE size #-}
size :: MonadSTM stm => Bimap stm leftKey rightKey -> stm Int
size (Bimap leftMap _) =
  A.size leftMap

-- |
-- Focus on a right value by the left value.
--
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can look up an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focusLeft #-}
focusLeft :: (MonadSTM stm, Hashable leftKey, Hashable rightKey) => B.Focus rightKey stm result -> leftKey -> Bimap stm leftKey rightKey -> stm result
focusLeft rightFocus leftKey (Bimap leftMap rightMap) =
  do
    ((output, change), maybeRightKey) <- A.focus (B.extractingInput (B.extractingChange rightFocus)) leftKey leftMap
    case change of
      B.Leave ->
        return ()
      B.Remove ->
        forM_ maybeRightKey $ \oldRightKey -> A.delete oldRightKey rightMap
      B.Set newRightKey ->
        do
          forM_ maybeRightKey $ \rightKey -> A.delete rightKey rightMap
          maybeReplacedLeftKey <- A.focus (B.lookup <* B.insert leftKey) newRightKey rightMap
          forM_ maybeReplacedLeftKey $ \replacedLeftKey -> A.delete replacedLeftKey leftMap
    return output

-- |
-- Focus on a left value by the right value.
--
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can look up an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focusRight #-}
focusRight :: (MonadSTM stm, Hashable leftKey, Hashable rightKey) => B.Focus leftKey stm result -> rightKey -> Bimap stm leftKey rightKey -> stm result
focusRight valueFocus2 rightKey (Bimap leftMap rightMap) =
  focusLeft valueFocus2 rightKey (Bimap rightMap leftMap)

-- |
-- Look up a right value by the left value.
{-# INLINE lookupLeft #-}
lookupLeft :: (MonadSTM stm, Hashable leftKey) => leftKey -> Bimap stm leftKey rightKey -> stm (Maybe rightKey)
lookupLeft leftKey (Bimap leftMap _) =
  A.lookup leftKey leftMap

-- |
-- Look up a left value by the right value.
{-# INLINE lookupRight #-}
lookupRight :: (MonadSTM stm, Hashable rightKey) => rightKey -> Bimap stm leftKey rightKey -> stm (Maybe leftKey)
lookupRight rightKey (Bimap _ rightMap) =
  A.lookup rightKey rightMap

-- |
-- Insert the association by the left value.
{-# INLINE insertLeft #-}
insertLeft :: (MonadSTM stm, Hashable leftKey, Hashable rightKey) => rightKey -> leftKey -> Bimap stm leftKey rightKey -> stm ()
insertLeft rightKey =
  focusLeft (B.insert rightKey)

-- |
-- Insert the association by the right value.
{-# INLINE insertRight #-}
insertRight :: (MonadSTM stm, Hashable leftKey, Hashable rightKey) => leftKey -> rightKey -> Bimap stm leftKey rightKey -> stm ()
insertRight leftKey rightKey (Bimap leftMap rightMap) =
  insertLeft leftKey rightKey (Bimap rightMap leftMap)

-- |
-- Delete the association by the left value.
{-# INLINE deleteLeft #-}
deleteLeft :: (MonadSTM stm, Hashable leftKey, Hashable rightKey) => leftKey -> Bimap stm leftKey rightKey -> stm ()
deleteLeft leftKey (Bimap leftMap rightMap) =
  A.focus B.lookupAndDelete leftKey leftMap
    >>= mapM_ (\rightKey -> A.delete rightKey rightMap)

-- |
-- Delete the association by the right value.
{-# INLINE deleteRight #-}
deleteRight :: (MonadSTM stm, Hashable leftKey, Hashable rightKey) => rightKey -> Bimap stm leftKey rightKey -> stm ()
deleteRight rightKey (Bimap leftMap rightMap) =
  deleteLeft rightKey (Bimap rightMap leftMap)

-- |
-- Delete all the associations.
{-# INLINE reset #-}
reset :: MonadSTM stm => Bimap stm leftKey rightKey -> stm ()
reset (Bimap leftMap rightMap) =
  do
    A.reset leftMap
    A.reset rightMap

-- |
-- Stream associations actively.
--
-- Amongst other features this function provides an interface to folding.
{-# INLINE unfoldlM #-}
unfoldlM :: MonadSTM stm => Bimap stm leftKey rightKey -> UnfoldlM stm (leftKey, rightKey)
unfoldlM (Bimap leftMap _) =
  A.unfoldlM leftMap

-- |
-- Stream the associations passively.
{-# INLINE listT #-}
listT :: Bimap STM key value -> ListT STM (key, value)
listT (Bimap leftMap _) =
  A.listT leftMap
