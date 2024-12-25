module adv_fs.Day21

open Puz.Input

type IHasLoc =
    abstract member loc: int * int

type NavPadInput =
    | Up
    | Down
    | Left
    | Right
    | Activate

    interface IHasLoc with
        member k.loc =
            match k with
            | Left -> (0, 0)
            | Down -> (1, 0)
            | Right -> (2, 0)
            | Up -> (1, 1)
            | Activate -> (2, 1)

    member k.ToString =
        match k with
        | Up -> "^"
        | Down -> "v"
        | Left -> "<"
        | Right -> ">"
        | Activate -> "A"

type KeyPadInput =
    | K0
    | K1
    | K2
    | K3
    | K4
    | K5
    | K6
    | K7
    | K8
    | K9
    | A

    static member fromChar c =
        match c with
        | '0' -> K0
        | '1' -> K1
        | '2' -> K2
        | '3' -> K3
        | '4' -> K4
        | '5' -> K5
        | '6' -> K6
        | '7' -> K7
        | '8' -> K8
        | '9' -> K9
        | 'A' -> A
        | _ -> failwith "Unknown char"

    interface IHasLoc with
        member k.loc =
            match k with
            | K0 -> (1, 0)
            | K1 -> (0, 1)
            | K2 -> (1, 1)
            | K3 -> (2, 1)
            | K4 -> (0, 2)
            | K5 -> (1, 2)
            | K6 -> (2, 2)
            | K7 -> (0, 3)
            | K8 -> (1, 3)
            | K9 -> (2, 3)
            | A -> (2, 0)


type Day21() =
    let input = readText "input/input21.txt"

    let testInput =
        "029A\n\
        980A\n\
        179A\n\
        456A\n\
        379A"

    let parseOne (line: string) =
        [ KeyPadInput.fromChar line[0]
          KeyPadInput.fromChar line[1]
          KeyPadInput.fromChar line[2]
          KeyPadInput.fromChar line[3] ]

    let parse input =
        input |> readLines |> Array.map parseOne |> List.ofArray

    let pathTo (from: IHasLoc) (target: IHasLoc) =
        let (fromX, fromY) = from.loc
        let (toX, toY) = target.loc
        let y = toY - fromY
        let x = toX - fromX

        let vertical =
            if y < 0 then List.replicate -y Down
            elif y = 0 then []
            else List.replicate y Up

        let horizontal =
            if x < 0 then List.replicate -x Left
            elif x = 0 then []
            else List.replicate x Right

        let r = List.concat [ vertical; horizontal; [ NavPadInput.Activate ] ]
        let s = r |> List.map (_.ToString) |> String.concat ""
        printfn $"{from} -> {target}: {s}"
        r

    let robotPath targetPath =
        NavPadInput.Activate :: targetPath
        |> List.windowed 2
        |> List.collect (fun l -> pathTo l[0] l[1])

    let evaluateMove from goal =
        let numPadPath = pathTo from goal

        let robot1Path = robotPath numPadPath

        robot1Path

    //let robot2Path = robotPath robot1Path

    //let robot3Path = robotPath robot2Path

    //let personPath = robotPath robot2Path

    //personPath

    let evaluateCode (code: string) =
        let keys =
            [ KeyPadInput.A
              KeyPadInput.fromChar code[0]
              KeyPadInput.fromChar code[1]
              KeyPadInput.fromChar code[2]
              KeyPadInput.fromChar code[3] ]

        let roboPath1 = keys |> List.windowed 2 |> List.collect (fun l -> pathTo l[0] l[1])
        let str1 = roboPath1 |> List.map (_.ToString) |> String.concat ""
        printfn $"\n{str1}"

        let roboPath2 =
            (Activate :: roboPath1)
            |> List.windowed 2
            |> List.collect (fun l -> pathTo l[0] l[1])

        let str2 = roboPath2 |> List.map (_.ToString) |> String.concat ""
        printfn $"{str2}"

        let roboPath3 =
            (Activate :: roboPath2)
            |> List.windowed 2
            |> List.collect (fun l -> pathTo l[0] l[1])

        let str3 = roboPath3 |> List.map (_.ToString) |> String.concat ""
        printfn $"{str3}"
        //roboPath3

        // let path = keys |> List.windowed 2 |> List.collect (fun l -> evaluateMove l[0] l[1])
        // let str = path |> List.map (fun k -> k.ToString) |> String.concat ""
        // printfn $"\n{code}:\n{str}"
        // printfn "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"
        (int code[0..2]) * roboPath3.Length

    interface Day with
        member this.DayName = "21"
        member this.answer1 = "?"
        member this.answer2 = "?"

        member this.part1() =
            let codes = testInput |> readLines |> List.ofArray
            let paths = codes[0..0] |> List.map evaluateCode
            paths |> List.sum |> string

        member this.part2() = "!"
