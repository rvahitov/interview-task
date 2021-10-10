namespace InterviewProblem

type GridRow = int
type GridColumn = int
type GridCell = GridRow * GridColumn

module GridCell =
    /// checks if other cell connected on left or on top
    let isJuniorNeighbour cell otherCell = let row, col = cell in otherCell = (row, col - 1) || otherCell = (row - 1, col)
    let to_str cell = let row, col = cell in $"%d{row},%d{col}"

type Grid = int [] []

module Grid =
    /// generates grid with dimension filled with random 0s or 1s
    let generate dimension =
        let random = System.Random()
        Array.init dimension (fun _ -> Array.init dimension (fun _ -> if random.Next() % 3 = 0 then 1 else 0))

    /// get grids dimension
    let dimension (grid: Grid) = Array.length grid

    /// get cells value of grid
    let cellValue (cell: GridCell) (grid: Grid) =
        let row, column = cell
        grid.[row].[column]

    /// finds all connected cells
    let getConnectedCells (grid: Grid) =
        let gridDimension = dimension grid

        let appendToConnected cell lst =
            let indexedList = lst |> List.indexed

            let indexesToRemove, listToAppend =
                indexedList
                |> List.filter (snd >> List.exists (GridCell.isJuniorNeighbour cell))
                |> List.fold (fun (indexes, lists) (index, l) -> (index :: indexes, lists @ l)) ([], [])
                |> function
                    | ixs, [] -> ixs, [ cell ]
                    | ixs, l -> ixs, cell :: l

            indexedList
            |> List.filter (fun (index, _) -> List.contains index indexesToRemove |> not)
            |> List.map snd
            |> List.append [ listToAppend ]


        let rec loop cell acc =
            match cell with
            | row, _ when row = gridDimension -> acc
            | row, col when col = gridDimension -> loop (row + 1, 0) acc
            | row, col when cellValue (row, col) grid = 0 -> loop (row, col + 1) acc
            | row, col ->
                let nextLoop = loop (row, col + 1)
                appendToConnected cell acc |> nextLoop

        loop (0, 0) [] |> List.map List.rev |> List.rev
