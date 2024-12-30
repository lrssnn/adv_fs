module adv_fs.Day23

open Puz.Input
open Puz.Convenience

type Day23() =
    let input = readText "input/input23.txt"

    let testInput =
        "kh-tc\n\
        qp-kh\n\
        de-cg\n\
        ka-co\n\
        yn-aq\n\
        qp-ub\n\
        cg-tb\n\
        vc-aq\n\
        tb-ka\n\
        wh-tc\n\
        yn-cg\n\
        kh-ub\n\
        ta-co\n\
        de-co\n\
        tc-td\n\
        tb-wq\n\
        wh-td\n\
        ta-ka\n\
        td-qp\n\
        aq-cg\n\
        wq-ub\n\
        ub-vc\n\
        de-ta\n\
        wq-aq\n\
        wq-vc\n\
        wh-yn\n\
        ka-de\n\
        kh-ta\n\
        co-tc\n\
        wh-qp\n\
        tb-vc\n\
        td-yn"

    let parseNetwork input =
        input
        |> readLines
        |> Array.fold
            (fun network line ->
                let (a, b) = splitOnce '-' line
                let aDests = b :: (Map.tryFind a network |> Option.defaultValue [])
                let bDests = a :: (Map.tryFind b network |> Option.defaultValue [])
                network |> Map.add a aDests |> Map.add b bDests)
            Map.empty

    let connectsTo network a b =
        network |> Map.find a |> List.contains b

    let findTriplesFrom network (from: string) =
        network
        |> Map.find from
        |> List.collect (fun first ->
            network
            |> Map.find first
            |> List.choose (fun second ->
                if connectsTo network from second then
                    Some([ from; first; second ] |> List.sort)
                else
                    None))

    interface Day with
        member this.DayName = "23"
        member this.answer1 = "1358"
        member this.answer2 = "?"


        member this.part1() =
            let network = parseNetwork input

            let loops =
                network |> Map.keys |> Seq.collect (findTriplesFrom network) |> Set.ofSeq

            loops
            |> Set.filter (fun l -> l |> List.exists (fun com -> com.StartsWith("t")))
            |> Set.count
            |> string

        member this.part2() = "!"
