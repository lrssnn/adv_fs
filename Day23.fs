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

    let rec detectLoop (start: string) (acc: string list) (network: Map<string, string list>) : string list list =
        if acc.Length > 1 && acc.Head = start then
            [ acc ]
        else
            network
            |> Map.find acc.Head
            |> List.collect (fun dest -> detectLoop start (dest :: acc) network)

    let addAll set list =
        list |> List.fold (fun s e -> Set.add e s) set

    let rec collectNetwork network seed segment queue =
        let q = addAll queue (Map.find seed network)
        let toProcess = Set.difference q segment

        if Set.isEmpty toProcess then
            // We have collected all the elements
            segment
        else
            // We haven't, pop an element from the queue and ???
            let next = toProcess.MinimumElement
            collectNetwork network next (Set.add next segment) toProcess

    interface Day with
        member this.DayName = "23"
        member this.answer1 = "?"
        member this.answer2 = "?"


        member this.part1() =
            let network = parseNetwork testInput

            let loops =
                network.Keys
                |> Seq.map (fun start -> collectNetwork network start Set.empty Set.empty)
                |> List.ofSeq

            "!"

        member this.part2() = "!"
