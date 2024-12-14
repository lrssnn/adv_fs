module adv_fs.Day13

open System
open Puz.Input
open FSharpx.String

let modFactor = 10000000000000L

type Button = { X: int64; Y: int64 }

type Prize =
    { X: int64
      Y: int64 }

    static member modify p : Prize =
        { p with
            X = p.X + modFactor
            Y = p.Y + modFactor }

type Game =
    { A: Button
      B: Button
      Prize: Prize }

    static member modify g = { g with Prize = Prize.modify g.Prize }

type Day13() =
    let input = readText "input/input13.txt"

    let parseButton line : Button =
        let firstPlus = line |> indexOfString "+"
        let lastPlus = line |> lastIndexOfString "+"
        let comma = line |> indexOfString ","
        let x = line[(firstPlus + 1) .. (comma - 1)] |> int64
        let y = line[(lastPlus + 1) ..] |> int64
        { X = x; Y = y }

    let parsePrize line : Prize =
        let firstEquals = line |> indexOfString "="
        let lastEquals = line |> lastIndexOfString "="
        let comma = line |> indexOfString ","
        let x = line[(firstEquals + 1) .. (comma - 1)] |> int64
        let y = line[(lastEquals + 1) ..] |> int64
        { X = x; Y = y }

    let parseOne input =
        let lines = input |> splitChar' [| '\n' |] 3
        let a = parseButton lines[0]
        let b = parseButton lines[1]
        let prize = parsePrize lines[2]
        { A = a; B = b; Prize = prize }

    let parse input =
        input |> splitString [| "\n\n" |] StringSplitOptions.None |> Array.map parseOne

    let trySolveGame game =
        // Using stolen maths to solve the optimisation problem with linear algebra
        let aPushes, aRemainder =
            Int64
                .DivRem(
                    ((game.Prize.Y * game.B.X) - (game.Prize.X * game.B.Y)),
                    ((game.A.Y * game.B.X) - (game.A.X * game.B.Y))
                )
                .ToTuple()

        if not (aRemainder = 0) then
            None
        else
            let bPresses, bRemainder =
                Int64.DivRem((game.Prize.X - aPushes * game.A.X), game.B.X).ToTuple()

            if not (bRemainder = 0) then
                None
            else
                Some((aPushes * 3L) + bPresses)

    let solve input modify =
        input
        |> parse
        |> fun arr -> if modify then arr |> Array.map Game.modify else arr
        |> Array.choose trySolveGame
        |> Array.sum

    interface Day with
        member this.DayName = "13"
        member this.answer1 = "39290"
        member this.answer2 = "73458657399094"

        member this.part1() = solve input false |> string
        member this.part2() = solve input true |> string
