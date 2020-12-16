namespace December2014

module Exam = 

    // Problem 1
    type Rel<'a,'b> = ('a * 'b list) list

    let rel: Rel<int, string> = [(1, ["a"; "b"; "c"]); (4, ["b"; "e"])]
    let rel1: Rel<int, string> = [(1, ["a"]); (2, ["b"])]
    let list1 = [(2, "c"); (1, "a"); (2, "b")]

    // 1.1
    let rec apply x rel =
        match rel with
            | [] -> []
            | (a, blist)::_ when a = x -> blist 
            | _::tail -> apply x tail 

    // 1.2    
    let rec inRelationAux y blist =
        match blist with 
            | [] -> false
            | head::_ when y = head -> true
            | _::tail -> inRelationAux y tail

    let rec inRelation x y rel =
        match rel with
            | [] -> false
            | (a, blist)::_ when a = x -> inRelationAux y blist 
            | _::tail -> inRelation x y tail

    
    // 1.3
    let rec insert x y (rel : Rel<'a,'b>) : Rel<'a,'b> =
        match rel with
            | [] -> []
            | (a, blist)::tail when a = x -> (a, y::blist)::tail
            | head::tail -> head::(insert x y tail)
 
    // 1.4
    let toRel list : Rel<'a,'b> =
        let rec toRelAux list result : Rel<'a,'b> =
            match list with
            | [] -> result
            | (number, letter)::tail when List.isEmpty (apply number result) -> toRelAux tail ((number, [letter])::result)
            | (number, letter)::tail -> toRelAux tail (insert number letter result)
   
        toRelAux list []

    
    // Problem 2

    // 2.1
    let multTable n =
        seq {n .. 10*n}
        
    // 2.2
    let tableOf m n f =
        seq { 
            for a in 1..m do
                for b in 1..n do
                    yield (a, b, f a b) 
        }

    // 2.3
    let infiniteStrings (char : string) =
        Seq.initInfinite (fun v -> String.replicate (v + 1) char ) 

    
    // 2.4
    let rec f i = function
        | [] -> []
        | x::xs -> (x+i)::f (i*i) xs

    // 2.5
    // Tail recursive
    let rec fTailRec i inList res =
        match inList with 
            | [] -> res
            | x::xs -> fTailRec (i*i) xs (x+1::res)

    // Continuation-based Tail-recursive
    let rec fConBasedTailRec i c inList =
        match inList with 
            | [] -> c []
            | x::xs -> fConBasedTailRec (i*i) (fun v -> c(x+i::v)) xs

    
    // Problem 4
    type Outcome = | S | F // S for success, F for failure
    type Sample = Outcome list
    type ProbTree = | Branch of string * float * ProbTree * ProbTree
                    | Leaf of string

    let exp = Branch(">2", 0.67, Branch(">3", 0.5, Leaf "A", Leaf "B"), Branch(">3", 0.5, Leaf "C", Leaf "D"))
    
    // 4.1
    let rec probOK probTree =
        match probTree with
            | Leaf _ -> true
            | Branch(_, prob, _, _) when prob < 0.0 || prob > 1.0 -> false
            | Branch(_, _, branch1, branch2) -> probOK branch1 && probOK branch2

    
    // 4.2
    let rec isSample(os, t) =
        match os, t with
            | [], Leaf _ -> true
            | head::tail, Branch(_, _, _, branch2) when head = F -> isSample(tail, branch2)
            | head::tail, Branch(_, _, branch1, _) when head = S -> isSample(tail, branch1)
            | _ -> false

    
    // 4.3
    type Description = (Outcome * string) list * float * string

    let descExp = ([(F, ">2"); (S, ">3")], 0.165, "C")

    let descriptionOf os t =
        let rec descriptionOfAux os t (osList, probResult, osResult) =
            match os, t with
                | [], Leaf value -> List.rev osList, probResult, value
                | head::tail, Branch(word, prob, branch1, _) when head = S -> descriptionOfAux tail branch1 ((head, word)::osList, probResult * prob, osResult)
                | head::tail, Branch(word, prob, _, branch2) when head = F -> descriptionOfAux tail branch2 ((head, word)::osList, probResult * (1.0 - prob), osResult)
                | _ -> failwith "sample is not correct"

        if not (isSample(os, t)) then failwith "sample is not correct"
        else  descriptionOfAux os t ([], 1.0, "")

    
    // 4.4
    // let rec getOs tree = 
    //     match tree with 
    //         | Leaf value -> [value]
    //         | Branch(_, _, branch1, branch2) -> getOs branch1 @ getOs branch2

    // let rec allDescriptions tree =
    //     let os = getOs tree
    //     descriptionOf os tree

    // 4.4
    let rec allDescriptions tree =
        let rec allDescriptionsAux tree treeNotModified result =
            match tree with
                | Leaf _ -> Set.empty.Add(descriptionOf (List.rev result) treeNotModified)
                | Branch(_, _, branch1, branch2) -> Set.union (allDescriptionsAux branch1 treeNotModified (S::result)) (allDescriptionsAux branch2 treeNotModified (F::result))
    
        allDescriptionsAux tree tree []

    
    // 4.5
    let probabilityOf t pred =
        let rec probabilityOfAux t pred prob =
            let (letter, boolean) = pred
            match t with
                | Leaf value when letter = value && boolean -> prob
                | Leaf value when letter = value && not boolean -> prob
                | Branch(_, p, branch1, branch2) -> (probabilityOfAux branch1 pred (prob * p)) + (probabilityOfAux branch2 pred (prob * (1.0 - p)))
                | _ -> 0.0

        probabilityOfAux t pred 1.0


    [<EntryPoint>]
    let main argv =

        printfn "Apply 1 rel -> %A" (apply 1 rel)
        printfn "Apply 1 rel -> %A" (apply 0 rel)

        printfn "inRelation 4 'e' rel -> %A" (inRelation 4 "e" rel)
        printfn "inRelation 1 'e' rel -> %A" (inRelation 1 "e" rel)

        printfn "insert 2 'c' [(1, ['a']); (2, ['b'])] -> %A" (insert 2 "c" rel1)

        printfn "toRel list1 -> %A" (toRel list1)

        printfn "tableof 3 4 (+) -> %A" (tableOf 3 4 (+))
        printfn "infinite strings -> %A" (infiniteStrings "a")

        printfn "Prob ok of exp -> %A" (probOK exp)
        printfn "Description of [F;S] -> %A" (descriptionOf [F;S] exp)
        printfn "All Descriptions of exp tree -> %A" (allDescriptions exp)

        printfn "Probability of B on exp tree -> %A" (probabilityOf exp ("B", true))

        
        0;;