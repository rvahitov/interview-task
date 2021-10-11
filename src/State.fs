namespace InterviewProblem

/// Model state
module ModelState =
    type T = { GridDimension: int; Grid: Grid; ConnectedCells: GridCell list list }

    /// creates new state with provided grid dimension
    let create dimension =
        let grid = Grid.generate dimension
        let connectedCells = Grid.getConnectedCells grid
        { GridDimension = dimension; Grid = grid; ConnectedCells = connectedCells }

    /// creates state with default dimension
    let init () = create 5

    let gridDimension { GridDimension = value } = value
    let grid { Grid = value } = value
    let connectedCells { ConnectedCells = value } = value

    /// get connected cells for provided cell
    let getConnectedCells cell state =
        let cellValue = state |> grid |> Grid.cellValue cell

        if cellValue = 0 then
            []
        else
            state
            |> connectedCells
            |> List.tryFind (List.contains cell)
            |> Option.defaultValue []
            |> List.except [ cell ]

    /// get count of connected cells for provided cell
    let getConnectedCellsCount cell state = state |> getConnectedCells cell |> List.length |> ((+) 1)


/// UI state
module UIState =
    type T =
        {
            ZeroCellColor: string
            OneCellColor: string
            ConnectedCellsColor: string
            SelectedCell: GridCell option
            HoveredCells: GridCell list
        }

    /// creates default state
    let init () =
        {
            ZeroCellColor = "white"
            OneCellColor = "#ff4040"
            ConnectedCellsColor = "#800040"
            SelectedCell = None
            HoveredCells = []
        }

    let zeroCellColor { ZeroCellColor = value } = value
    let withZeroCellColor value state = { state with ZeroCellColor = value }
    let oneCellColor { OneCellColor = value } = value
    let withOneCellColor value state = { state with OneCellColor = value }
    let connectedCellsColor { ConnectedCellsColor = value } = value
    let withConnectedCellsColor value state = { state with ConnectedCellsColor = value }

    let selectedCell { SelectedCell = value } = value
    let withSelectedCell value state = { state with SelectedCell = value }
    let hoveredCells { HoveredCells = value } = value
    let withHoveredCells value state = { state with HoveredCells = value }

/// Applications state
module AppState =
    type T = { ModelState: ModelState.T; UIState: UIState.T }

    let create dimension = { ModelState = ModelState.create dimension; UIState = UIState.init () }

    let init () = create 5

    let modelState { ModelState = value } = value
    let withModelState modelState state = { state with ModelState = modelState }
    let uiState { UIState = value } = value
    let withUIState uiState state = { state with UIState = uiState }

    let apply message state =
        let onGridDim dimension =
            if dimension = (state |> modelState |> ModelState.gridDimension) then
                state
            else
                state |> withModelState (ModelState.create dimension)

        let onGridCelSel cell =
            let uiState = state |> uiState

            match uiState |> UIState.selectedCell with
            | Some sc when sc = cell -> state
            | _ -> state |> withUIState (uiState |> UIState.withSelectedCell (Some cell))

        let onCelHov cells = state |> withUIState (state |> uiState |> UIState.withHoveredCells cells)

        let onFilCelCol color = state |> withUIState (state |> uiState |> UIState.withOneCellColor color)

        let onConCelCol color = state |> withUIState (state |> uiState |> UIState.withConnectedCellsColor color)
        Message.fold onGridDim onGridCelSel onCelHov onFilCelCol onConCelCol message
