module AppStateTests

open FsCheck
open InterviewProblem
open global.Xunit

[<Fact>]
let ``Get modelState should return value from state`` () =
    let test state =
        (state |> AppState.modelState) = state.ModelState
    Check.QuickThrowOnFailure test

[<Fact>]
let ``Get uiState should return value from state`` () =
    let test state =
        (state |> AppState.uiState) = state.UIState
    Check.QuickThrowOnFailure test

[<Fact>]
let ``withModelState should return new state with provided ModelState`` () =
    let test state modelState =
        (state |> AppState.withModelState modelState) = {state with ModelState = modelState}
    Check.QuickThrowOnFailure test

[<Fact>]
let ``withUIState should return new state with provided UIState`` () =
    let test state uiState =
        (state |> AppState.withUIState uiState) = {state with UIState = uiState}
    Check.QuickThrowOnFailure test
    
[<Fact>]
let ``apply ChangeGridDimension on state should return new state with dimension on ModelState``() =
    let test state (dimension: PositiveInt) =
        let newState = (state |> AppState.apply (Message.ChangeGridDimension dimension.Get))
        (newState |> AppState.modelState |> ModelState.gridDimension) = dimension.Get
    Check.QuickThrowOnFailure test
        
[<Fact>]
let ``apply GridCellSelected on state should return new state with selectedCell on UIState``() =
    let test state selectedCell =
        let newState = (state |> AppState.apply (Message.GridCellSelected selectedCell))
        (newState |> AppState.uiState |> UIState.selectedCell) = Some selectedCell
    Check.QuickThrowOnFailure test
