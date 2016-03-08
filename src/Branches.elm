module Branches (MergeResult(..), Commit, initialCommit, commit, merge) where

import Set exposing (Set)


type MergeResult id
  = FastForward (List id)
  | NoChange
  | AlreadyAhead
  | MergeTODO


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


merge : Commit comparable -> Commit comparable -> MergeResult comparable
merge (Commit remote) (Commit local) =
  if remote.id == local.id then
    NoChange
  else if Set.member remote.id local.all then
    AlreadyAhead
  else
    let
      findFastForwardChain commit acc =
        case commit.parents of
          [] ->
            Just acc
          [Commit singleParent] ->
            if Set.member singleParent.id local.all then
              Just <| commit.id :: acc
            else
              findFastForwardChain singleParent (commit.id :: acc)
          _ ->
            Nothing
    in
      case findFastForwardChain remote [] of
        Just newCommits ->
          FastForward newCommits
        Nothing ->
          MergeTODO
