namespace Polynomial

open System

module Program = 

    let rec addPol(list1, list2) =
        match (list1, list2) with
        | ([], []) -> []
        | (x::tail, []) -> x::addPol(tail, [])
        | ([], x::tail) -> x::addPol([], tail)
        | (x::tail, y::tail2) -> x+y::addPol(tail, tail2)

    
    let rec mulC(cons, list2) =
        match (list2) with
        | ([]) -> []
        | (x::tail) -> (x*cons)::mulC(cons, tail)


    // Modified for Part 3: it makes sure the Pol is legal after the sub result
    let rec sub(list1, list2) = 
        match (list1, list2) with
            | ([], []) -> []
            | ([], x::tail) -> x::sub([], tail)
            | (x::tail, []) -> x::sub(tail, [])
            | (x::tail, y::tail2) -> x-y::sub(tail, tail2)

     
    let rec mulX(list) = 
        0::list


    let rec mul(list1, list2) = 
        match (list1, list2) with
            | ([], _) ->  []
            | (head::list1, list2) -> addPol(mulC(head, list2), mulX(mul(list1,list2)))


    let rec eval(a, list) =
        match (list) with 
            | [] -> []
            | x::tail -> a*x::eval(a, tail)



    [<EntryPoint>]
    let main argv =

        let list1 = [1;2;3]
        let list2 = [4;5;6;7;8]

        let list3 = [2;3;0;1]
        let list4 = [1;0;2;3]

        let cons = 2

        let list = addPol(list1, list2)
        let mulc = mulC(3, [1;0;2;3])
        let resta = sub(list1, list2)
        let mulx = mulX(list1)
        let evals = eval(cons, list1)

        printfn "Aqui va la lista: %A" list
        printfn "Multiplicado %A" mulc
        printfn "Resta %A" resta
        printfn "Resta ilegal %A" (sub([1;0;3;4], [0;2;2;4]))
        printfn "mulx %A" mulx
        printfn "mul polynomial %A" (mul(list3, list4))
        printfn "Eval %A" evals
        // addPol(list1, list2)
        0;; // return an integer exit code