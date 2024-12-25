module adv_fs.Day22

open Puz.Input

type Day22() =
    let input = readText "input/input22.txt"

    let testInput =
        "1\n\
        10\n\
        100\n\
        2024"

    let parse input =
        input |> readLines |> Array.map int64 |> List.ofArray

    let mix a b = a ^^^ b

    let prune a = a % 16777216L

    let nextSecret num =
        let a = num |> mix (num * 64L) |> prune
        let b = a |> mix (a / 32L) |> prune
        b |> mix (b * 2048L) |> prune

    let iterate iterations seed =
        [ 1..iterations ] |> List.fold (fun acc _loop -> nextSecret acc) seed


    interface Day with
        member this.DayName = "22"
        member this.answer1 = "?"
        member this.answer2 = "?"


        member this.part1() =
            parse input |> List.map (iterate 2000) |> List.sum |> string

        member this.part2() = "!"
