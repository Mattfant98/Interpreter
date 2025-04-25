type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string
type command = ADD | SUB | MUL | DIV | PUSH of stackValue

let interpreter (input, output) =
    let ic = open_in input
    in

    let oc = open_out output
    in

    let rec loop_read acc = 
        try
            let l = String.trim(input_line ic) in loop_read (l::acc)
        with
            | End_of_file -> List.rev acc
    in

    let strList = loop_read []
    in

    let str2com s =
        match s with
            | "Add" -> ADD
            | "Sub" -> SUB
            | "Mul" -> MUL
            | "Div" -> DIV
    in

    let comList = List.map str2com strList
    in

    let rec processor cl stack =
        match (cl, stack) with
            |(ADD::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands (INT(a+b)::restOfStack)
            |(ADD::restOfCommands, stack_) -> ERROR::stack

    in processor comList []