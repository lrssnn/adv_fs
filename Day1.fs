module adv_fs.Day1

open System
open Puz.Input
open FSharpx.String

type Day1() =
    let input = readText "input/input01.txt"

    let lists input =
        input
        |> splitChar [| '\n' |]
        |> Array.map (splitString' [| "   " |] 2 StringSplitOptions.None)
        |> Array.map (fun arr -> (int arr[0], int arr[1]))
        |> Array.unzip

    let solve a b =
        let a = Array.sort a
        let b = Array.sort b
        let pairs = Array.zip a b
        pairs |> Array.map (fun (x, y) -> abs (y - x)) |> Array.sum

    let solve2 (a: int array) (b: int array) =
        let matches = b |> Array.countBy id |> Map

        a
        |> Array.map (fun num -> num * (Map.tryFind num matches |> Option.defaultValue 0))
        |> Array.sum

    interface Day with
        member this.DayName = "01"
        member this.answer1 = "1222801"
        member this.answer2 = "22545250"

        member this.part1() =
            let (a, b) = lists input
            string (solve a b)

        member this.part2() =
            let (a, b) = lists input
            string (solve2 a b)
