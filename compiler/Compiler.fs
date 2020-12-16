namespace Compiler

type Instruction = | ADD | SUB | SIGN | ABS | PUSH of int
type Stack = int List
type Exp = X, C -2, C 7, Abs X, Minus(C 7), Add(Abs(Minus(C 7)), Sub(X, Minus(Add(C 2, X))))

module Compiler = 

    let addStack(stack : Stack, sign : string) = 
        match sign with 
            | "+" -> (stack.Head + stack.Tail.Head) :: stack.Tail.Tail
            | "-" -> (stack.Head - stack.Tail.Head) :: stack.Tail.Tail
            | _ -> []


    // intpInstr: Stack -> Instruction -> Stack
    let intpInstr(stack, instruction) : Stack =  

        match instruction with 
            | ADD -> addStack(stack, "+")
            | SUB -> addStack(stack, "-")
            | SIGN -> ((-1)*stack.Head) :: stack.Tail
            | ABS -> (abs(stack.Head)) :: stack.Tail
            | PUSH element -> element :: stack

    
    // exec: Instruction list -> int
    let exec (ins:Instruction list) : int = (List.fold (fun a -> intpInstr a) [] ins).Head


    //sem : Exp → int → int
    let sem(exp : Exp, number : int) : int = 1
        

    let compile(exp : Exp, number : int) : Instruction list = 1

    
    // exec(prg) = sem(e, x)

    [<EntryPoint>]
    let main argv =

        let stack1 = [4;5;6;7;8]
        let stack2 = [-4;5;6;7;8]
        let element = 200


        printfn "ADD [4;5;6;7;8]: %A" (intpInstr(stack1, ADD))
        printfn "SUB [4;5;6;7;8]: %A" (intpInstr(stack1, SUB))
        printfn "SIGN [4;5;6;7;8]: %A" (intpInstr(stack1, SIGN))
        printfn "ABS [-4;5;6;7;8]: %A" (intpInstr(stack2, ABS))
        printfn "PUSH [4;5;6;7;8]: %A" (intpInstr(stack1, PUSH element))
        0;;