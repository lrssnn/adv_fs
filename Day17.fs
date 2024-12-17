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
        | "3" -> Literal 2
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
      buffer: string list }

    static member fromLines(lines: string list) =
        let a = lines[0][(lastIndexOfString " " lines[0]) + 1 ..] |> int
        let b = lines[1][(lastIndexOfString " " lines[1]) + 1 ..] |> int
        let c = lines[2][(lastIndexOfString " " lines[2]) + 1 ..] |> int

        let program =
            lines[4][(indexOfString " " lines[4]) + 1 ..]
            |> splitChar [| ',' |]
            |> List.ofArray
            |> List.windowed 2
            |> List.map (fun window -> Instruction.fromPair window[0] window[1])

        { RegA = a
          RegB = b
          RegC = c
          ip = 0
          program = program
          buffer = List.empty }
        
    member c.adv op = { c with ip = c.ip + 2; RegA = c.RegA / (pown 2 op) }     
    member c.bxl op = { c with ip = c.ip + 2; RegB = c.RegB ^^^ op }
    member c.bst op =  { c with ip = c.ip + 2; RegB = op % 8 }
    member c.jnz op = { c with ip = if c.RegA = 0 then c.ip + 2 else op }
    member c.bxc op = { c with ip = c.ip + 2; RegC = c.RegB ^^^ c.RegC }
    member c.out op = { c with ip = c.ip + 2; buffer = op :: c.buffer }
    member c.bdv op = { c with ip = c.ip + 2; RegB = c.RegA / (pown 2 op) }
    member c.cdv op = { c with ip = c.ip + 2; RegC = c.RegA / (pown 2 op) }
    
    member c.getValue op =
        match op with
        | Literal x -> x
        | Register r ->
            match r with
            | A -> c.RegA
            | B -> c.RegB
            | C -> c.RegC
            
    member c.tick instruction =
        match instruction with
        | Adv op -> c.adv (c.getValue op)
        | Bxl op -> c.bxl (c.getValue op)
        | Bst op -> c.bst (c.getValue op)
        | Jnz op -> c.jnz (c.getValue op)
        | Bxc op -> c.bxc (c.getValue op)
        | Out op -> c.out (c.getValue op)
        | Bdv op -> c.bdv (c.getValue op)
        | Cdv op -> c.cdv (c.getValue op)
    
    member c.tick =
        if c.ip >= c.program.Length then
            c, true
        else
            let instruction = c.program[c.ip]
            c.process instruction, false
            

    member c.toString =
        let program = c.program |> List.map string |> String.concat ","
        let buffer = c.buffer |> String.concat ","
        $"A: {c.RegA}\nB: {c.RegB}\nC: {c.RegC}\nip: {c.ip}\nPrg: {program}\nOutput: {buffer}"

type Day17() =
    let input = readText "input/input17.txt"

    let testInput =
        "Register A: 729\n\
        Register B: 0\n\
        Register C: 0\n\
\n\
        Program: 0,1,5,4,3,0"

    interface Day with
        member this.DayName = "17"
        member this.answer1 = "?"
        member this.answer2 = "?"

        member this.part1() =
            let computer = Computer.fromLines (testInput |> readLinesList)
            computer.toString

        member this.part2() = "!"
