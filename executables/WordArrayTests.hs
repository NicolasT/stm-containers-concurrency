{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import STMContainers.Prelude
import STMContainers.Transformers
import qualified STMContainers.WordArray as WordArray
import qualified WordArrayTests.Update as Update


main = htfMain $ htf_thisModulesTests

prop_differentInterpretersProduceSameResults (update :: Update.Update Char ()) =
  Update.interpretMaybeList update ==
  fmap WordArray.toMaybeList (Update.interpretWordArray update)

prop_fromListIsIsomorphicToToList =
  forAll gen prop
  where
    gen = do
      indices <- (nub . sort) <$> listOf index
      mapM (liftA2 (flip (,)) char . pure) indices
      where
        index = choose (0, pred (WordArray.maxSize)) :: Gen Int
        char = arbitrary :: Gen Char
    prop list = 
      list === (WordArray.toList . WordArray.fromList) list
      