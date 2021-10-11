module UIStateTests

open FsCheck
open InterviewProblem
open global.Xunit

[<Fact>]
let ``Get zeroCellColor should return value from state`` () =
    let test state = (state |> UIState.zeroCellColor) = state.ZeroCellColor
    Check.QuickThrowOnFailure test

[<Fact>]
let ``Get oneCellColor should return value from state`` () =
    let test state = (state |> UIState.oneCellColor) = state.OneCellColor
    Check.QuickThrowOnFailure test

[<Fact>]
let ``Get connectedCellsColor should return value from state`` () =
    let test state = (state |> UIState.connectedCellsColor) = state.ConnectedCellsColor
    Check.QuickThrowOnFailure test

[<Fact>]
let ``Get selectedCell should return value from state`` () =
    let test state = (state |> UIState.selectedCell) = state.SelectedCell
    Check.QuickThrowOnFailure test

[<Fact>]
let ``Get hoveredCells should return value from state`` () =
    let test state = (state |> UIState.hoveredCells) = state.HoveredCells
    Check.QuickThrowOnFailure test

[<Fact>]
let ``withZeroCellColor should return new state with provided ZeroCellColor`` () =
    let test state newZeroCellColor = (state |> UIState.withZeroCellColor newZeroCellColor |> UIState.zeroCellColor) = newZeroCellColor
    Check.QuickThrowOnFailure test

[<Fact>]
let ``withOneCellColor should return new state with provided OneCellColor`` () =
    let test state newColor = (state |> UIState.withOneCellColor newColor |> UIState.oneCellColor) = newColor
    Check.QuickThrowOnFailure test

[<Fact>]
let ``withConnectedCellsColor should return new state with provided ConnectedCellsColor`` () =
    let test state newColor = (state |> UIState.withConnectedCellsColor newColor |> UIState.connectedCellsColor) = newColor
    Check.QuickThrowOnFailure test


[<Fact>]
let ``withSelectedCell should return new state with provided SelectedCell`` () =
    let test state selectedCell = (state |> UIState.withSelectedCell selectedCell) = { state with SelectedCell = selectedCell }
    Check.QuickThrowOnFailure test

[<Fact>]
let ``withHoveredCells should return new state with provided HoveredCells`` () =
    let test state hoveredCells = (state |> UIState.withHoveredCells hoveredCells) = { state with HoveredCells = hoveredCells }
    Check.QuickThrowOnFailure test
