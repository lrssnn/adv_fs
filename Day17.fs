module adv_fs.Day17

open Puz.Input
open FSharpx.String

type Reg =
    | A
    | B
    | C

type Combo =
    | Literal of int
    | Register of Reg

    static member fromStr str =
        match str with
        | "0" -> Literal 0
        | "1" -> Literal 1
        | "2" -> Literal 2
        | "3" -> Literal 3
        | "4" -> Register A
        | "5" -> Register B
        | "6" -> Register C
        | "7" -> failwith "invalid operand"
        | _ -> failwith "Unknown operand"

type Instruction =
    | Adv of Combo
    | Bxl of int
    | Bst of Combo
    | Jnz of int
    | Bxc of int
    | Out of Combo
    | Bdv of Combo
    | Cdv of Combo

    static member fromPair opcode operand =
        match opcode with
        | "0" -> Adv(Combo.fromStr operand)
        | "1" -> Bxl(int operand)
        | "2" -> Bst(Combo.fromStr operand)
        | "3" -> Jnz(int operand)
        | "4" -> Bxc(int operand)
        | "5" -> Out(Combo.fromStr operand)
        | "6" -> Bdv(Combo.fromStr operand)
        | "7" -> Cdv(Combo.fromStr operand)
        | _ -> failwith "Unknown opcode"

type Computer =
    { RegA: int
      RegB: int
      RegC: int
      ip: int
      program: Instruction list
      progStr: string
      buffer: string list }

    static member fromLines(lines: string list) =
        let a = lines[0][(lastIndexOfString " " lines[0]) + 1 ..] |> int
        let b = lines[1][(lastIndexOfString " " lines[1]) + 1 ..] |> int
        let c = lines[2][(lastIndexOfString " " lines[2]) + 1 ..] |> int

        let progString = lines[4][(indexOfString " " lines[4]) + 1 ..]

        let program =
            progString
            |> splitChar [| ',' |]
            |> List.ofArray
            |> List.windowed 2
            |> List.indexed
            |> List.filter (fun (i, _s) -> i % 2 = 0)
            |> List.map snd
            |> List.map (fun window -> Instruction.fromPair window[0] window[1])

        { RegA = a
          RegB = b
          RegC = c
          ip = 0
          program = program
          progStr = progString
          buffer = List.empty }

    member c.adv op =
        { c with
            ip = c.ip + 1
            RegA = c.RegA / (pown 2 op) }

    member c.bxl op =
        { c with
            ip = c.ip + 1
            RegB = (c.RegB ^^^ op) }

    member c.bst op = { c with ip = c.ip + 1; RegB = op % 8 }

    member c.jnz op =
        { c with
            ip = if c.RegA = 0 then c.ip + 1 else op } // op / 2, because we have compressed opcode operand into instruction

    member c.bxc op =
        { c with
            ip = c.ip + 1
            RegB = (c.RegB ^^^ c.RegC) }

    member c.out op =
        { c with
            ip = c.ip + 1
            buffer = (string (op % 8)) :: c.buffer }

    member c.bdv op =
        { c with
            ip = c.ip + 1
            RegB = c.RegA / (pown 2 op) }

    member c.cdv op =
        { c with
            ip = c.ip + 1
            RegC = c.RegA / (pown 2 op) }

    member c.getValue op =
        match op with
        | Literal x -> x
        | Register r ->
            match r with
            | A -> c.RegA
            | B -> c.RegB
            | C -> c.RegC

    member c.runInst instruction =
        match instruction with
        | Adv op -> c.adv (c.getValue op)
        | Bxl op -> c.bxl op
        | Bst op -> c.bst (c.getValue op)
        | Jnz op -> c.jnz op
        | Bxc op -> c.bxc op
        | Out op -> c.out (c.getValue op)
        | Bdv op -> c.bdv (c.getValue op)
        | Cdv op -> c.cdv (c.getValue op)

    member c.tick =
        if c.ip >= c.program.Length then
            c, true
        else
            let instruction = c.program[c.ip]
            c.runInst instruction, false

    member c.printBuf = c.buffer |> List.rev |> String.concat ","

    member c.toString =
        let program = c.program |> List.map string |> String.concat ","
        $"A: {c.RegA}\nB: {c.RegB}\nC: {c.RegC}\nip: {c.ip}\nPrg: {program}\nOutput: {c.printBuf}\nTarget: {c.progStr}"

type Day17() =
    let input = readText "input/input17.txt"

    let testInput =
        "Register A: 2024\n\
        Register B: 0\n\
        Register C: 0\n\
\n\
        Program: 0,3,5,4,3,0"

    let runUntilHalt (computer: Computer) =
        let mutable halted = false
        let mutable cp = computer

        while not halted do
            let com, halt = cp.tick
            cp <- com
            halted <- halt

        cp

    let runUntilHalt2 (computer: Computer) =
        let mutable halted = false
        let mutable cp = computer
        let mutable steps = 0

        while not halted do
            let com, halt = cp.tick
            cp <- com
            halted <- halt
            steps <- steps + 1

            if cp.printBuf.Length > 0 && not (cp.progStr.StartsWith(cp.printBuf)) then
                halted <- true

        cp, (cp.progStr = cp.printBuf), steps

    let findAns (computer: Computer) =
        let mutable input = 0
        let mutable foundIt = false

        let mutable best = 0

        while not foundIt do
            let _com, f, steps = runUntilHalt2 { computer with RegA = input }
            foundIt <- f
            input <- input + 1

            if steps > best then
                printfn $"{input}: {steps} ({_com.buffer.Length} / {_com.program.Length * 2}"
                best <- steps

        input - 1

    interface Day with
        member this.DayName = "17"
        member this.answer1 = "6,5,7,4,5,7,3,1,0"
        member this.answer2 = "?"

        member this.part1() =
            let computer = Computer.fromLines (input |> readLinesList)
            let state = runUntilHalt computer
            state.printBuf

        member this.part2() =
            let computer = Computer.fromLines (input |> readLinesList)
            let ans = findAns computer
            ans |> string
