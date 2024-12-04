module adv_fs.Day3

open Puz.Input
open System.Text.RegularExpressions

type Day3() =
    let input = readText "input/input03.txt"

    /// Remove everything between a don't() and a do()
    /// .*? is a non-greedy version of .*, which makes don't()xdo()ydo() remove
    /// only the x part instead of x and y. SingleLine prevents the match from resetting over the boundary
    /// of newlines, newlines mean nothing in this challenge
    let preprocess memory =
        Regex.Replace(memory, "don't\(\).*?do\(\)", "", RegexOptions.Singleline)

    /// Take one match of the mul(x, y) construct and return x * y
    let multiply (matched: Match) =
        // Group 0 is the entire matched string, 1 and 2 are the two capture groups
        (int matched.Groups[1].Value) * (int matched.Groups[2].Value)

    /// Return the sum of the result of all multiplication commands found in the memory string
    let solve memory =
        Regex.Matches(memory, "mul\((\d+),(\d+)\)") |> Seq.map multiply |> Seq.sum

    interface Day with
        member this.DayName = "03"
        member this.answer1 = "174960292"
        member this.answer2 = "56275602"

        member this.part1() = string (solve input)

        member this.part2() = string (solve (preprocess input))
