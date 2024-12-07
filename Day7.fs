module adv_fs.Day7

open Puz.Input
open Puz.Convenience
open FSharpx.String

type Day7() =
    let input = readText "input/input07.txt"

    /// Turn one line of input into a target number and the numbers we have to work with
    let parse input =
        readLines input
        |> Array.map (fun line ->
            let (left, right) = splitOnceStr ": " line
            let target = int64 left
            let nums = splitChar [| ' ' |] right |> Array.map int64
            (target, nums))

    /// Concatenate the two number values i.e. concat 12 34 = 1234
    let concat a b = (string a) + string (b) |> int64

    /// Determine if the number 'target' can be reached by combining the 'current' value, and the remaining 'nums'
    /// Nums can be folded into 'current' by either addition or multiplication,
    /// or string concatenation if the 'withConcat' flag is set
    let rec isSatisfiable withConcat target nums current =
        // Small optimisation, all our operators make the number bigger, so we can give up early in some cases
        if current > target then
            false
        // Once we have used all the numbers, check if we ended up in the right place
        else if Array.length nums = 0 then
            current = target
        // If there are any numbers yet, check the two or three options for a result, consuming one item from the list
        else
            let nextStep = (isSatisfiable withConcat target (Array.tail nums))

            (nextStep (current + Array.head nums))
            || (nextStep (current * Array.head nums))
            || (withConcat && nextStep (concat current (Array.head nums)))

    /// Solve the challenge by parsing the input, filtering by the satisfiable equations, then summing by the
    /// target number.
    let solve withConcat =
        let items = parse input

        items
        // Note that we move the first item from nums into the accumulator here, if we start with 0 in the accumulator
        // multiplication will be broken
        |> Array.filter (fun (target, nums) -> isSatisfiable withConcat target (Array.tail nums) (Array.head nums))
        |> Array.sumBy fst

    interface Day with
        member this.DayName = "06"
        member this.answer1 = "1298103531759"
        member this.answer2 = "140575048428831"

        member this.part1() = solve false |> string
        member this.part2() = solve true |> string
