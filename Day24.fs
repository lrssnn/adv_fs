module adv_fs.Day24

open System
open FSharpx.String
open Puz.Input
open Puz.Convenience

type Wire = { label: string; value: bool }

type Op =
    | AND
    | OR
    | XOR

    static member ofStr str =
        match str with
        | "AND" -> AND
        | "OR" -> OR
        | "XOR" -> XOR
        | _ -> failwith "Invalid Op string"

    member o.apply a b =
        match o with
        | AND -> a && b
        | OR -> a || b
        | XOR -> a <> b

type Gate =
    { inA: string
      inB: string
      out: string
      op: Op }

type Day24() =
    let input = readText "input/input24.txt"

    let testInput =
        "x00: 1\n\
x01: 0\n\
x02: 1\n\
x03: 1\n\
x04: 0\n\
y00: 1\n\
y01: 1\n\
y02: 1\n\
y03: 1\n\
y04: 1\n\
\n\
ntg XOR fgs -> mjb\n\
y02 OR x01 -> tnw\n\
kwq OR kpj -> z05\n\
x00 OR x03 -> fst\n\
tgd XOR rvg -> z01\n\
vdt OR tnw -> bfw\n\
bfw AND frj -> z10\n\
ffh OR nrd -> bqk\n\
y00 AND y03 -> djm\n\
y03 OR y00 -> psh\n\
bqk OR frj -> z08\n\
tnw OR fst -> frj\n\
gnj AND tgd -> z11\n\
bfw XOR mjb -> z00\n\
x03 OR x00 -> vdt\n\
gnj AND wpb -> z02\n\
x04 AND y00 -> kjc\n\
djm OR pbm -> qhw\n\
nrd AND vdt -> hwm\n\
kjc AND fst -> rvg\n\
y04 OR y02 -> fgs\n\
y01 AND x02 -> pbm\n\
ntg OR kjc -> kwq\n\
psh XOR fgs -> tgd\n\
qhw XOR tgd -> z09\n\
pbm OR djm -> kpj\n\
x03 XOR y03 -> ffh\n\
x00 XOR y04 -> ntg\n\
bfw OR bqk -> z06\n\
nrd XOR fgs -> wpb\n\
frj XOR qhw -> z04\n\
bqk OR frj -> z07\n\
y03 OR x01 -> nrd\n\
hwm AND bqk -> z03\n\
tgd XOR rvg -> z12\n\
tnw OR pbm -> gnj"

    let parseWires input =
        input
        |> readLines
        |> Array.map (fun line ->
            let label, value = line |> splitOnceStr ": "
            { label = label; value = (value = "1") })
        |> List.ofArray

    let parseGates input =
        input
        |> readLines
        |> Array.map (fun line ->
            let left, out = line |> splitOnceStr " -> "
            let inA, rest = left |> splitOnce ' '
            let op, inB = rest |> splitOnce ' '

            { inA = inA
              inB = inB
              out = out
              op = Op.ofStr op })
        |> List.ofArray

    let parse input =
        let wiresS, gatesS = input |> splitOnceStr "\n\n"
        wiresS |> parseWires, gatesS |> parseGates

    let rec resolve wires gates loopSet target =
        if loopSet |> Set.contains target then
            None
        else
            //printfn $"Resolving {target}"

            match wires |> List.tryFind (fun w -> w.label = target) with
            | Some w -> Some w.value
            | None ->
                // Don't have it as a wire, find its gate and resolve the inputs
                let gate = gates |> List.find (fun g -> g.out = target)

                match (resolve wires gates (loopSet |> Set.add target) gate.inA) with
                | None -> None
                | Some a ->
                    match (resolve wires gates (loopSet |> Set.add target) gate.inB) with
                    | None -> None
                    | Some b -> Some(gate.op.apply a b)

    let iToZ i = $"z{(string i).PadLeft(2, '0')}"

    let getWires wires targetPrefix =
        wires |> List.filter (fun wire -> wire.label |> startsWith targetPrefix)

    let getWireValue wires label =
        getWires wires label
        |> Seq.map (fun w -> if w.value then "1" else "0")
        |> Seq.rev
        |> String.concat ""
        |> (fun binary -> Convert.ToInt64(binary, 2))

    let evaluateToBinary wires gates =
        Seq.initInfinite id
        |> Seq.takeWhile (fun idx -> gates |> List.exists (fun g -> g.out = iToZ idx))
        |> Seq.map iToZ
        |> Seq.map (resolve wires gates Set.empty)
        |> (fun opts ->
            if opts |> Seq.exists (fun opt -> opt.IsNone) then
                None
            else
                opts
                |> Seq.map Option.get
                |> Seq.map (fun bit -> if bit then "1" else "0")
                |> Seq.rev
                |> String.concat ""
                |> Some)


    let toGraphVis gate =
        $"{gate.inA} -> {gate.inA}{gate.op}{gate.inB}\n"
        + $"{gate.inB} -> {gate.inA}{gate.op}{gate.inB}\n"
        + $"{gate.inA}{gate.op}{gate.inB} -> {gate.out}"

    let toGraphVis gates =
        gates |> List.map toGraphVis |> String.concat "\n"

    let rec insertions x =
        function
        | [] -> [ [ x ] ]
        | (y :: ys) as l -> (x :: l) :: (List.map (fun x -> y :: x) (insertions x ys))

    let rec permutations =
        function
        | [] -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

    interface Day with
        member this.DayName = "24"
        member this.answer1 = "57632654722854"
        member this.answer2 = "?"

        member this.part1() =
            let wires, gates = parse input
            // Resolve z0x wires until we can't find them any more
            let binary = evaluateToBinary wires gates |> Option.get

            //printfn $"{binary}"
            Convert.ToInt64(binary, 2) |> string

        member this.part2() =
            let wires, gates = parse input
            printfn $"{toGraphVis gates}"

            let x = getWireValue wires "x"
            let y = getWireValue wires "y"

            let targetZ = x + y
            let targetBinary = $"%B{targetZ}"
            printfn $"{x} + {y} = {targetZ}"

            printfn $"%046B{x}"
            printfn $"%046B{y}"
            printfn "="
            printfn $"%046B{targetZ}"

            let realBinary = evaluateToBinary wires gates
            printfn $"{realBinary}"

            // Generate the list of swaps...
            // Two swaps means taking four items
            let last = gates.Length - 1

            // let answer =
            //     [| 0..last |]
            //     |> Array.tryFind (fun i ->
            //         printfn $"{i}"
            //
            //         [| (i + 1) .. last |]
            //         |> Array.Parallel.exists (fun j ->
            //             [ 0..last ]
            //             |> Seq.exists (fun k ->
            //                 [ (k + 1) .. last ]
            //                 |> Seq.exists (fun l ->
            //                     if i = j || i = k || i = l || j = k || j = l || k = l then
            //                         false
            //                     else
            //                         let g =
            //                             gates
            //                             |> List.indexed
            //                             |> List.map (fun (idx, element) ->
            //                                 if idx = i then { element with out = gates[j].out }
            //                                 elif idx = j then { element with out = gates[i].out }
            //                                 elif idx = k then { element with out = gates[l].out }
            //                                 elif idx = l then { element with out = gates[k].out }
            //                                 else element)
            //
            //                         // if l % 10 = 0 then
            //                         //     printfn $"{i},{j},{k},{l}"
            //
            //                         match evaluateToBinary wires g with
            //                         | None -> false
            //                         | Some binary ->
            //                             let z = Convert.ToInt64(binary, 2)
            //
            //                             if z = targetZ then
            //                                 printfn $"Found answer {i},{j},{k},{l}"
            //                                 true
            //                             else
            //                                 false))))

            "?" |> string
