module Tests (..) where

import ElmTest exposing (..)
import Branches exposing (..)


foldl a b c =
  List.foldl a c b


commits first rest =
  initialCommit first
    |> foldl commit rest


all : Test
all =
  suite
    "BranchingData"
    [ merge
        (commits "A" [])
        (commits "A" [])
        |> assertEqual (NoChange)
        |> test "no new commits"
    , merge
        (commits "A" [])
        (commits "A" [ "B" ])
        |> assertEqual (AlreadyAhead)
        |> test "new commits on local"
    , merge
        (commits "A" [ "B" ])
        (commits "A" [])
        |> assertEqual (FastForward [ "B" ])
        |> test "new commits on remote"
    ]
