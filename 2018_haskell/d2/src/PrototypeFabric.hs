module PrototypeFabric where

import Input

data Acc = Acc { match :: String
               , diff :: String }

getPrototypeFabricBox :: [BoxID] -> Maybe String
getPrototypeFabricBox [] = Nothing
getPrototypeFabricBox (b:bx) =
  case checkBox b bx of
    Nothing -> getPrototypeFabricBox bx
    a -> a

checkBox :: BoxID -> [BoxID] -> Maybe String
checkBox x (y:xs) =
  case compareBoxes x y Acc { match = "", diff = "" } of
    Nothing -> checkBox x xs
    a -> a
checkBox _ _ = Nothing

compareBoxes :: BoxID -> BoxID -> Acc -> Maybe String
compareBoxes _ _ Acc { diff = (_:_:_)} = Nothing
compareBoxes "" "" acc = Just $ match acc
compareBoxes (x:xs) (y:ys) acc =
  if x == y
     then compareBoxes xs ys acc { match = match acc ++ [x] }
     else compareBoxes xs ys acc { diff = diff acc ++ [x] }
compareBoxes _ _ _ = Nothing
