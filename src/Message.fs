namespace InterviewProblem

module Message =
    type T =
        | ChangeGridDimension of int
        | GridCellSelected of GridCell
        | GridCellsHovered of GridCell list
        | FilledCellColor of string
        | ConnectedCellsColor of string

    let fold onChangeDim onCelSel onCelHov onFillCol onConCol =
        function
        | ChangeGridDimension v -> onChangeDim v
        | GridCellSelected v -> onCelSel v
        | GridCellsHovered v -> onCelHov v
        | FilledCellColor v -> onFillCol v
        | ConnectedCellsColor v -> onConCol v
