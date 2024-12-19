module adv_fs.Day19

open System
open Puz.Input
open Puz.Convenience
open FSharpx.String

type Day19() =
    let input = readText "input/input19.txt"

    let parseOptions input =
        input |> splitString [| ", " |] StringSplitOptions.None |> List.ofArray

    let parseTargets input =
        input |> splitChar [| '\n' |] |> List.ofArray

    let parse input =
        let optionsStr, targetsStr = input |> splitOnceStr "\n\n"
        let options = parseOptions optionsStr
        let targets = parseTargets targetsStr
        options, targets

    let countWays strategy options target =
        if target = "" then
            1L
        else
            options
            |> List.filter (fun option -> startsWith option target)
            |> List.sumBy (fun option -> strategy options target[option.Length ..])

    let memoCountWays =
        // Memoisation storage
        let mutable map = Map.empty

        let rec countResult =
            fun options target ->
                let cacheOpt = map |> Map.tryFind target

                // If we have it in the cache, use it, otherwise generate the result and
                // Store it for next time
                match cacheOpt with
                | Some(value) -> value
                | None ->
                    let value = countWays countResult options target
                    map <- map |> Map.add target value
                    value

        countResult

    let solve options targets =
        targets
        |> List.filter (fun target -> memoCountWays options target > 0)
        |> List.length

    let solve2 options targets =
        targets |> List.sumBy (memoCountWays options)

    interface Day with
        member this.DayName = "19"
        member this.answer1 = "322"
        member this.answer2 = "715514563508258"

        member this.part1() =
            let options, targets = parse input
            solve options targets |> string

        member this.part2() =
            let options, targets = parse input
            solve2 options targets |> string
