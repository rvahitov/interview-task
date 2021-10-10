namespace InterviewProblem

open Feliz
open InterviewProblem

module ConfigurationView =
    /// renders grid dimension range
    let renderGridDimensionRange state dispatch =
        Html.div [ prop.children [ Html.label [ prop.htmlFor "grid_dimension_range"
                                                prop.className "form-label"
                                                prop.text "Grid dimension" ]
                                   Html.input [ prop.id "grid_dimension_range"
                                                prop.type' "range"
                                                prop.classes [ "form-control"
                                                               "form-range" ]
                                                prop.min 4
                                                prop.max 10
                                                prop.value (state |> AppState.modelState |> ModelState.gridDimension)
                                                prop.step 1
                                                prop.onChange (Message.ChangeGridDimension >> dispatch) ] ] ]

    /// renders color picker for filled cell
    let renderFillCellColorPicker state dispatch =
        Html.div [ prop.children [ Html.label [ prop.htmlFor "fill_cell_col_picker"
                                                prop.className "form-label"
                                                prop.text "Filled cell color" ]
                                   Html.input [ prop.id "fill_cell_col_picker"
                                                prop.type' "color"
                                                prop.classes [ "form-control"
                                                               "form-control-color" ]
                                                prop.value (state |> AppState.uiState |> UIState.oneCellColor)
                                                prop.onChange (Message.FilledCellColor >> dispatch) ] ] ]

    /// renders color picker for connected (hover) cells
    let renderConnectedCellColorPicker state dispatch =
        Html.div [ prop.children [ Html.label [ prop.htmlFor "connect_cell_col_picker"
                                                prop.className "form-label"
                                                prop.text "Connected cells color" ]
                                   Html.input [ prop.id "connect_cell_col_picker"
                                                prop.type' "color"
                                                prop.classes [ "form-control"
                                                               "form-control-color" ]
                                                prop.value (state |> AppState.uiState |> UIState.connectedCellsColor)
                                                prop.onChange (Message.ConnectedCellsColor >> dispatch) ] ] ]


    /// renders configuration part of the page
    let render state dispatch =
        Html.div [ Html.div [ prop.className "row"
                              prop.children [ renderGridDimensionRange state dispatch ] ]
                   Html.div [ prop.className "row"
                              prop.children [ renderFillCellColorPicker state dispatch ] ]
                   Html.div [ prop.className "row"
                              prop.children [ renderConnectedCellColorPicker state dispatch ] ] ]

module GridView =

    /// render content of the cells if any
    let private renderCellContent cell state =
        let selectedCell = state |> AppState.uiState |> UIState.selectedCell

        let connectedCount =
            selectedCell
            |> Option.filter ((=) cell)
            |> Option.map (fun selectedCell -> state |> AppState.modelState |> ModelState.getConnectedCellsCount selectedCell |> sprintf "%d")

        let contentStyle =
            [
                match connectedCount with
                | Some _ -> style.visibility.visible
                | None -> style.visibility.collapse
                style.color "white"
                style.display.inlineBlock
            ]

        Html.span [ prop.style contentStyle
                    prop.text (connectedCount |> Option.defaultValue "") ]

    /// renders cell
    let private renderCellBox (cell: GridCell) state dispatch =
        let grid = state |> AppState.modelState |> ModelState.grid
        let uiState = state |> AppState.uiState
        let cellValue = grid |> Grid.cellValue cell

        let bgColor =
            match cellValue, uiState |> UIState.hoveredCells with
            | 0, _ -> uiState |> UIState.zeroCellColor
            | _, lst when lst |> List.contains cell -> uiState |> UIState.connectedCellsColor
            | _ -> uiState |> UIState.oneCellColor

        let cellBoxStyles = [ style.backgroundColor bgColor ]

        let onClick _ = if cellValue <> 0 then Message.GridCellSelected cell |> dispatch

        Html.div [ prop.style cellBoxStyles
                   prop.classes [ "border"
                                  "border-dark"
                                  "text-center" ]
                   prop.key (GridCell.to_str cell)
                   prop.children [ renderCellContent cell state ]
                   prop.onClick onClick
                   prop.onMouseEnter
                       (fun _ ->
                           state
                           |> AppState.modelState
                           |> ModelState.getConnectedCells cell
                           |> Message.GridCellsHovered
                           |> dispatch) ]

    /// renders grid
    let renderGrid state dispatch =
        let dimension = state |> AppState.modelState |> ModelState.gridDimension

        let gridStyles =
            [
                style.width 500
                style.height 500
                style.display.grid
                style.gridTemplateColumns (List.init dimension (fun _ -> length.fr dimension))
                style.gridTemplateRows (List.init dimension (fun _ -> length.fr dimension))
            ]

        Html.div [ prop.className "container"
                   prop.style gridStyles
                   prop.children [ for row in 0 .. dimension - 1 do
                                       for col in 0 .. dimension - 1 -> renderCellBox (row, col) state dispatch ]
                   prop.onMouseLeave (fun _ -> Message.GridCellsHovered [] |> dispatch) ]


module MainView =
    /// renders application page
    let render state dispatch =
        Html.div [ Html.div [ prop.className "row"

                              prop.children [ Html.div [ prop.className "col-4"
                                                         prop.children [ ConfigurationView.render state dispatch ] ]
                                              Html.div [ prop.className "col-8"
                                                         prop.children [ GridView.renderGrid state dispatch ] ] ] ] ]
