module adv_fs.Day22

open Puz.Input

type Day22() =
    let input = readText "input/input22.txt"

    let parse input =
        input |> readLines |> Array.map int64 |> List.ofArray

    let mix a b = a ^^^ b

    let prune a = a % 16777216L

    let nextSecret num =
        let a = num |> mix (num * 64L) |> prune
        let b = a |> mix (a / 32L) |> prune
        b |> mix (b * 2048L) |> prune

    let nextPrice num =
        let s = nextSecret num
        let price = s % 10L
        price, s

    let iterate iterations seed =
        [ 1..iterations ] |> List.fold (fun acc _loop -> nextSecret acc) seed

    // Evaluate the score of the given array of (difference * price) pairs, looking for the sequence of
    // changes i, j, k, l
    let evaluate (diffsPrices: (int64 * int64) array) i j k l =
        [ 3 .. diffsPrices.Length - 1 ]
        |> List.tryPick (fun last ->
            if
                fst diffsPrices[last - 3] = i
                && fst diffsPrices[last - 2] = j
                && fst diffsPrices[last - 1] = k
                && fst diffsPrices[last] = l
            then
                Some(snd diffsPrices[last])
            else
                None)
        |> Option.defaultValue 0L

    interface Day with
        member this.DayName = "22"
        member this.answer1 = "14180628689"
        member this.answer2 = "1690"

        member this.part1() =
            parse input |> List.map (iterate 2000) |> List.sum |> string

        member this.part2() =
            let seeds = parse input |> Array.ofList

            let diffsPrices: (int64 * int64) array array =
                seeds
                // Generate the 2000 numbers for each seed (initial value plus 1999 extras)
                |> Array.map (fun seed -> [| 1..1999 |] |> Array.scan (fun last _i -> nextSecret last) seed)
                // Turn the numbers into prices, just the last digit
                |> Array.map (fun nums -> nums |> Array.map (fun n -> n % 10L))
                // Attach the differences, keeping the prices because we need that for the answer
                |> Array.map (fun prices -> prices |> Array.windowed 2 |> Array.map (fun w -> (w[1] - w[0], w[1])))

            // Generate all of the perms
            let lim = 9

            let perms =
                [| -lim .. lim |]
                |> Array.collect (fun i ->
                    [| -lim .. lim |]
                    |> Array.collect (fun j ->
                        [| -lim .. lim |]
                        |> Array.collect (fun k -> [| -lim .. lim |] |> Array.map (fun l -> i, j, k, l))))

            // For each perm, get the score, then take the max score for the answer
            perms
            |> Array.map (fun (i, j, k, l) -> diffsPrices |> Array.Parallel.sumBy (fun seed -> evaluate seed i j k l))
            |> Array.Parallel.max
            |> string
