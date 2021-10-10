namespace InterviewProblem

open Elmish
open Elmish.React

module App =
    Program.mkSimple AppState.init AppState.apply MainView.render
    |> Program.withReactSynchronous "app"
    |> Program.run
