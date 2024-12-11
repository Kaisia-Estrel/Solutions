{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module UtilsTuple where

import Language.Haskell.TH qualified as TH

nTupleT :: Int -> TH.Name -> TH.Type
nTupleT n a = apply n (TH.TupleT n)
  where
    apply 0 tup = tup
    apply m tup = TH.AppT (apply (m - 1) tup) (TH.VarT a)

class ListToTuple (n :: Nat) a t | n a -> t where
  listToTuple :: [a] -> t

mapTupleInstance :: Int -> TH.DecsQ
mapTupleInstance n = do
  a <- TH.newName "a"
  b <- TH.newName "b"
  f <- TH.newName "f"

  let ta = nTupleT n a
  let tb = nTupleT n b

  names <- replicateM n (TH.newName "x")
  let tuplePat = TH.TupP $ map TH.VarP names
  let tupleExp = TH.TupE $ map (\x -> Just $ TH.VarE f `TH.AppE` TH.VarE x) names

  let mapTupleN = TH.mkName $ "mapTuple" <> show n
  let
    changeName (TH.FunD _ clause) = TH.FunD mapTupleN clause
    changeName (TH.SigD _ ty) = TH.SigD mapTupleN ty
    changeName x = x
  fmap changeName
    <$> [d|
      mapTuple :: ($(TH.varT a) -> $(TH.varT b)) -> $(pure ta) -> $(pure tb)
      mapTuple $(TH.varP f) $(pure tuplePat) = $(pure tupleExp)
      |]

listToTupleInstance :: Int -> TH.DecsQ
listToTupleInstance n = do
  let nType = TH.LitT (TH.NumTyLit (fromIntegral n))
  typeName <- TH.newName "a"

  let classType =
        TH.ConT (TH.mkName "ListToTuple")
          `TH.AppT` nType
          `TH.AppT` TH.VarT typeName
          `TH.AppT` nTupleT n typeName

  fnDefinitions <- do
    names <- replicateM n (TH.newName "x")
    let listPat = TH.ListP $ map TH.VarP names
    let tupleExp = TH.TupE $ map (Just . TH.VarE) names
    let errorMessage =
          TH.VarE (TH.mkName "error")
            `TH.AppE` TH.LitE (TH.StringL $ "Invalid list size for tuple of size " ++ show n)
    let failingAction = TH.Clause [TH.WildP] (TH.NormalB errorMessage) []
    let matchingAction =
          TH.FunD
            (TH.mkName "listToTuple")
            [TH.Clause [listPat] (TH.NormalB tupleExp) [], failingAction]
    return [matchingAction]

  return $ pure $ TH.InstanceD Nothing [] classType fnDefinitions
