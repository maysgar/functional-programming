namespace Polynomial

module Partwo =

    // Part 4:
    type Poly = int list

    type Degree = 
        | MinusInf 
        | FinN of int

    let deg(list: int list) = 

        let rec deg(list, degree) : Degree =
            match list with
            | [] -> Degree.MinusInf
            | [last] -> Degree.FinN (degree)
            | head::tail -> deg(tail, degree+1)

        deg(list, 0)


    let addD(degree1, degree2) : Degree = 
        match(degree1, degree2) with
            | (Degree.MinusInf, _) -> Degree.MinusInf
            | (_, Degree.MinusInf) -> Degree.MinusInf
            | (Degree.FinN a, Degree.FinN b) -> Degree.FinN (a + b)



    // Define a new function to print a name.
    // It is defined above the main function.
    let printGreeting name =
        printfn "Hello %s from F#!" name

    let rec isLegal(list) = 
        match (list) with
            | [] -> true
            | [0] -> false
            | (head::list) ->  isLegal(list)

    let rec reverse(list) = 
        match (list) with 
            | [] -> []
            | head::tail -> reverse(tail) @ [head]

    let rec auxPrune(list) = 
        match list with
            | [] -> []
            | 0::list -> auxPrune(list)
            | list -> list

    
    let rec isPrune(list) = 
        reverse(auxPrune(reverse(list)))

    let rec toString(list) = 

        let rec toStringRec(list, pos) =
            match (list, pos) with
                | ([], _) -> ""
                | (head::tail, 0) -> string (head:int) + toStringRec(tail, 1)
                | (head::tail, pos) -> " + " + string (head:int) + "x^" + string pos + toStringRec(tail, pos+1)
        
        toStringRec(list, 0)

    let rec derivative(list) =

        let rec derivativeRec(list, pos) =
            match (list, pos) with
                | ([], _) -> []
                | (head::tail, 0) -> derivativeRec(tail, 1) 
                | (head::tail, 1) -> head::derivativeRec(tail, 2)
                | (head::tail, pos) -> (head*pos)::derivativeRec(tail, pos+1) 

        derivativeRec(list, 0)

    
    // Modified for Part 3: it makes sure the Pol is legal after the sum result
    let addPol(list1, list2) =    

        let rec addPolRec(list1, list2) =
            match (list1, list2) with
            | ([], []) -> []
            | (x::tail, []) -> x::addPolRec(tail, [])
            | ([], x::tail) -> x::addPolRec([], tail)
            | (x::tail, y::tail2) -> x+y::addPolRec(tail, tail2)

        isPrune(addPolRec(list1, list2))


    // Modified for Part 3: it makes sure the Pol is legal after the sub result
    let sub(list1, list2) =

        let rec subRec(list1, list2) = 
            match (list1, list2) with
                | ([], []) -> []
                | ([], x::tail) -> x::subRec([], tail)
                | (x::tail, []) -> x::subRec(tail, [])
                | (x::tail, y::tail2) -> x-y::subRec(tail, tail2)

        isPrune(subRec(list1, list2))

    // Modified for Part 3: return an empty Pol is constant is 0
    let rec mulC(cons, list2) =
        match (cons, list2) with
            | (_, []) -> []
            | (0, _) -> []
            | (cons, x::tail) -> (x*cons)::mulC(cons, tail)
    

    let rec mulX(list) = 
        0::list

    // Part 3: We always expect a legal result (addPol, mulC & mulX are all legal)
    let rec mul(list1, list2) = 
        match (list1, list2) with
            | ([], _) ->  []
            | (head::list1, list2) -> addPol(mulC(head, list2), mulX(mul(list1,list2)))


    // Modified for Part 3: it makes sure the Pol is legal after eval
    let eval(a, list) =

        let rec evalRec(a, list) =
            match (list) with 
                | [] -> []
                | x::tail -> a*x::evalRec(a, tail)

        isPrune(evalRec(a, list))


    let rec polPow(pol, n) =
        match (pol,n) with 
            | (_, -1) -> pol
            | (_, 0) -> pol
            | (pol, n) -> mul(pol, (polPow(pol, (n-1))))


    let rec compose(list1, list2) =

        let rec composeRec(list1, list2, pos) = 
            match (list1) with
                | (head::tail) when pos = 0 -> head::(composeRec(tail, list2, (pos+1))).[1..]
                | (head::tail) -> addPol(mulC(head,(polPow(list2,(pos-1)))), composeRec(tail, list2, (pos+1)))
                | [last] -> mulC(last, polPow(list1, pos))
                | [] -> []
        
        composeRec(list1, list2, 0)


    let ofList(list: int list) : Poly = 
        isPrune(list)

    let toList(poly: Poly) : int list = 
        poly
    
    // let addInv p1 p2 = isLegal(addPol(isPrune(p1),isPrune(p2)));; let _ = Check.Quick addInv;



    [<EntryPoint>]
    let main argv =

        let list1 = [1;2;3]
        let list2 = [4;5;6;7;8]

        let list3 = [2;3;0;1]
        let list4 = [1;2;3]

        let list5 = [1;2;3;0]

        let listPrune = [1;2;3;0;0;0;0]

        let listCompose1 = [2;0;0;4]
        let listCompose2 = [0;3;2]

        let cons = 2

        printfn "Legal -> %b" (isLegal(list1))
        printfn "Not legal -> %b" (isLegal(list5))
        printfn "List reversed -> %A" (reverse(list1))
        printfn "Apply prune -> %A" (isPrune(listPrune))
        printfn "Convert to string -> %A" (toString(list2))
        printfn "Derivative of [4;5;6;7;8] is: %A" (derivative(list2))
        printfn "Compose [2;0;0;4] and [0;3;2]: %A" (compose(listCompose1, listCompose2))
        printfn "Degree of polynomial: %A" (deg([1;0;0;2]))

        // addPol(list1, list2)
        0;; // return an integer exit code