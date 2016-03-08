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
    , suite
        "new commits on local"
        [ merge
            (commits "A" [])
            (commits "A" [ "B" ])
            |> assertEqual (AlreadyAhead)
            |> test "single commit after root"
        , merge
            (commits "A" [])
            (commits "A" [ "B", "C" ])
            |> assertEqual (AlreadyAhead)
            |> test "multiple commits"
        , merge
            (commits "A" [ "B" ])
            (commits "A" [ "B", "C" ])
            |> assertEqual (AlreadyAhead)
            |> test "single commit"
        ]
    , merge
        (commits "A" [ "B" ])
        (commits "A" [])
        |> assertEqual (FastForward [ "B" ])
        |> test "new commits on remote"
    ]
