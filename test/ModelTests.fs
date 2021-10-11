module ModelTests

open Swensen.Unquote
open Xunit

open InterviewProblem

[<Theory>]
[<InlineData(2, 2, 1, 2, true)>]
[<InlineData(3, 4, 3, 3, true)>]
[<InlineData(0, 1, 4, 5, false)>]
let ``Apply isJuniorNeighbour on cell otherCell should return expected value`` (cellRow, cellColl, otherRow, otherColl, result) = test <@ (GridCell.isJuniorNeighbour (cellRow, cellColl) (otherRow, otherColl)) = result @>

[<Fact>]
let ``Grid generate with dimension 5 should return 2D array with 5 rows and 5 columns`` () =
    let grid = Grid.generate 5
    let rows = Array.length grid
    let columns = grid |> Array.map Array.length
    let minColumn = columns |> Array.min
    let maxColumn = columns |> Array.max
    test <@ rows = minColumn && minColumn = maxColumn @>

[<Fact>]
let ``Grid generate with dimension 7 should return 2D array with 49 cells`` () =
    let cells = Grid.generate 7 |> Array.map Array.length |> Array.sum
    test <@ cells = 49 @>

[<Fact>]
let ``When I ask for dimension of the grid that was generated with 15 dimension I should receive 15 as the result`` () =
    let dimension = Grid.generate 15 |> Grid.dimension
    test <@ dimension = 15 @>

[<Fact>]
let ``Get cellValue should return 5`` () =
    let grid = [| [| 2; 4; 6 |]; [| 3; 1; 5 |]; [| 1; 1; 1 |] |]
    test <@ (grid |> Grid.cellValue (1, 2)) = 5 @>
