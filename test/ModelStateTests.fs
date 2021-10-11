module ModelStateTests

open Xunit
open Swensen.Unquote
open InterviewProblem

[<Fact>]
let ``Get gridDimension should return GridDimension from state`` () =
    let state = ModelState.create 10
    test <@ (state |> ModelState.gridDimension) = state.GridDimension @>

[<Fact>]
let ``Get grid should return Grid from state`` () =
    let state = ModelState.create 3
    test <@ (state |> ModelState.grid) = state.Grid @>

[<Fact>]
let ``Get connectedCells should return ConnectedCells from state`` () =
    let state = ModelState.create 4
    test <@ (state |> ModelState.connectedCells) = state.ConnectedCells @>
