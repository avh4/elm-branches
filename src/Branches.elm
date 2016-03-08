module Branches (..) where

import Set exposing (Set)


type MergeResult id
  = FastForward (List id)
  | NoChange
  | AlreadyAhead


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
    , parents = []
    , all = parent.all
    }


merge : Commit comparable -> Commit comparable -> MergeResult comparable
merge (Commit remote) (Commit local) =
  if remote.id == local.id then
    NoChange
  else if Set.member remote.id local.all then
    AlreadyAhead
  else
    FastForward [ remote.id ]
