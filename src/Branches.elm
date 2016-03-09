module Branches (MergeResult(..), Commit, initialCommit, commit, merge, join) where

import Set exposing (Set)


type MergeResult id
  = FastForward id id
  | NoChange
  | AlreadyAhead
  | Merge (Maybe id) id id


type Commit comparable
  = Commit
      { id : comparable
      , parents : List (Commit comparable)
      , all : Set comparable
      }


initialCommit : comparable -> Commit comparable
initialCommit new =
  Commit
    { id = new
    , parents = []
    , all = Set.singleton new
    }


commit : comparable -> Commit comparable -> Commit comparable
commit new (Commit parent) =
  Commit
    { id = new
    , parents = [ Commit parent ]
    , all = Set.insert new parent.all
    }


join : Commit comparable -> Commit comparable -> comparable -> Commit comparable
join (Commit left) (Commit right) new =
  Commit
    { id = new
    , parents = [ Commit left, Commit right ]
    , all =
        Set.union left.all right.all
          |> Set.insert new
    }


merge : Commit comparable -> Commit comparable -> MergeResult comparable
merge (Commit remote) (Commit local) =
  if remote.id == local.id then
    NoChange
  else if Set.member local.id remote.all then
    FastForward local.id remote.id
  else if Set.member remote.id local.all then
    AlreadyAhead
  else
    let
      findCommonAncestor commit =
        case commit.parents of
          [] ->
            Nothing

          [ Commit singleParent ] ->
            if singleParent.id == local.id then
              Just commit.id
            else if Set.member singleParent.id local.all then
              Just singleParent.id
            else
              findCommonAncestor singleParent

          _ ->
            Nothing
    in
      Merge (findCommonAncestor remote) remote.id local.id
