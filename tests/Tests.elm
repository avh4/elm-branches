module Tests (..) where

import ElmTest exposing (..)
import Branches exposing (..)


foldl a b c =
  List.foldl a c b


commits first rest =
  initialCommit first
    |> foldl commit rest


fork : ( a -> b, a -> c ) -> a -> ( b, c )
fork ( f1, f2 ) a =
  ( f1 a, f2 a )


join : comparable -> ( Commit comparable, Commit comparable ) -> Commit comparable
join mergeId ( left, right ) =
  Branches.join left right mergeId


assertMerge : MergeResult comparable -> ( Commit comparable, Commit comparable ) -> Assertion
assertMerge expected ( local, remote ) =
  assertEqual expected (Branches.merge remote local)


all : Test
all =
  suite
    "BranchingData"
    [ merge
        (commits "A" [])
        (commits "A" [])
        |> assertEqual (NoChange)
        |> test "no changes"
    , suite
        "changes on local"
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
    , suite
        "changes on remote"
        [ merge
            (commits "A" [ "B" ])
            (commits "A" [])
            |> assertEqual (FastForward "A" "B")
            |> test "single commit after root"
        , initialCommit "A"
            |> fork
                ( identity
                , commit "B"
                )
            |> assertMerge (FastForward "A" "B")
            |> test "single commit after root"
        , merge
            (commits "A" [ "B", "C" ])
            (commits "A" [ "B" ])
            |> assertEqual (FastForward "B" "C")
            |> test "single commit"
        , merge
            (commits "A" [ "B", "C", "D" ])
            (commits "A" [ "B" ])
            |> assertEqual (FastForward "B" "D")
            |> test "two commits"
        , merge
            (commits "A" [ "B", "C", "D", "E" ])
            (commits "A" [ "B" ])
            |> assertEqual (FastForward "B" "E")
            |> test "three commits"
        , initialCommit "A"
            |> fork
                ( identity
                , fork
                    ( commit "C"
                    , commit "D"
                    )
                    >> join "m1"
                )
            |> assertMerge (FastForward "A" "m1")
            |> test "containing merges"
        ]
    , suite
        "merges"
        [ merge
            (commits "A" [])
            (commits "B" [])
            |> assertEqual (Merge Nothing "A" "B")
            |> test "no common ancestors"
        , merge
            (commits "A" [ "B" ])
            (commits "A" [ "C" ])
            |> assertEqual (Merge (Just "A") "B" "C")
            |> test "common ancestor"
        ]
    ]
