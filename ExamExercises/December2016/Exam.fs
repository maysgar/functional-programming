namespace December2016

module Exam = 

    type Name = string
    type Event = string
    type Point = int
    type Score = Name * Event * Point

    type Scoreboard = Score list

    let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30); ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)]

    // 1.1
    let rec inv scoreboard =
        match scoreboard with
            | [] -> true
            | (_, _, score)::tail when score < 0 ->
                let (_, _, score2) = tail.Head
                if score < score2 || score < 0 then false
                else inv tail
            | (_, _, score)::tail -> inv tail

    // 1.2
    let rec insert s sb =
        let (name, event, point) = s
        match sb with
            | [] -> [s]
            | (name1, event1, score)::tail when score >= point -> (name1, event1, score) :: insert s tail
            | (name1, event1, score)::tail -> s :: (name1, event1, score) :: tail

    // 1.3
    let rec get(n, sb) = 
        match sb with
            | [] -> []
            | ((name, event, score)::tail) when name = n -> (event, score) :: get(n, tail) 
            | ((name, event, score)::tail) -> get(n, tail)

    // 1.4
    let rec top k sb =
        match k,sb with
            | k, _ when k = 0 -> Some []
            | k, _ when k < 0 -> None
            | _, [] -> None
            | (k, (name, event, score)::tail) ->
                match (top (k-1) tail) with 
                    | None -> None
                    | Some result -> Some ((name, event, score) :: result)

    // 2.1 & 2.2
    let rec replace a b xs =
        match xs with 
            | [] -> []
            | head::tail when head = a -> b :: replace a b tail
            | head::tail -> head :: replace a b tail

    // 2.3
    let replace2 a b xs =
        let rec replaceAux a b xs resultList = 
            match xs with
                | [] -> List.rev resultList
                | head::tail when head = a -> replaceAux a b tail (b::resultList)
                | _::tail -> replaceAux a b tail (a::resultList)
        
        replaceAux a b xs []

    
    // 4
    type Tree<'a,'b> = | A of 'a | B of 'b | Node of Tree<'a,'b> * Tree<'a,'b>

    // 4.1
    let tree1 = A true
    let tree2 = B [1]
    let tree3 = Node (tree1, tree2)
    
    let tree3T = Node (A true, B [1;2])

    // 4.2
    let rec countOcurrences tree =
        match tree with 
            | A _ -> 1
            | B _ -> 0 
            | Node (tree1, tree2) -> countOcurrences tree1 + countOcurrences tree2

    // 4.3
    let rec subst a a' b b' t =
        match t with 
            | A elem when elem = a -> A a'
            | B elem when elem = b -> B b' 
            | Node (tree1, tree2) -> Node (subst a a' b b' tree1, subst a a' b b' tree2)
            | leaf -> leaf

    // 4.4
    let rec g = function
        | Node(t1,t2) -> Node(g t2, g t1)
        | leaf -> leaf


    let rec f = function
        | A a -> ([a],[])
        | B b -> ([], [b])
        | Node(t1,t2) -> let (xs1,ys1) = f t1
                         let (xs2,ys2) = f t2
                         (xs1@xs2, ys1@ys2)

    // 4.5
    let rec fTailRecursive t result = 
        match t, result with
            | A a, (list1, list2) -> (list1@[([a],[])], list2)
            | B b, (list1, list2) -> (list1, list2@[([], [b])])
            | Node(t1,t2), _ -> let (res1, res2) = fTailRecursive t1 result
                                let (rest1, rest2) = fTailRecursive t2 result
                                (res1@rest1, res2@rest2)
    
    let rec fK t k = match t with
                        | A a         -> k([a],[])
                        | B b         -> k([], [b])
                        | Node(t1,t2) -> fK t1 (fun (xs1,ys1) -> fK t2 (fun (xs2,ys2) -> k(xs1@xs2,ys1@ys2)))


    // 5
    type T<'a> = N of 'a * T<'a> list

    let td = N("g", [])
    let tc = N("c", [N("d",[]); N("e",[td])])
    let tb = N("b", [N("c",[])])
    let ta = N("a", [tb; tc; N("f",[])])

    // 5.1
    let rec toListAux list = 
        match list with
            | [] -> []
            | [N (value, [])] -> [value]
            | N (value, list)::tail2 -> (value::toListAux list)@toListAux tail2

    let toList t =
        match t with 
            | N (value, []) -> [value]
            | N (value, list) -> value::toListAux list

    // 5.2
    let rec map f (N(v,ts)) = N(f v, List.map (map f) ts)

    type Path = int list

    // 5.3
    let rec isPath is t =
        match is, t with 
            | [], N (_) -> true
            | head::tail, N (_, treeList) -> try isPath tail treeList.[head]
                                             with _ -> false


    // 5.4  
    let rec get2 is t =
        match is, t with 
            | [], N (value, list) -> N (value, list)
            | head::tail, N (_, treeList) -> get2 tail treeList.[head]
    
    // 5.5
    let rec tryFindPathtoAux v treeList (path : int list) =
        match treeList, path with 
            | [], _ -> None
            | ((N (value, _))::_), _ when value = v -> Some (List.rev path)
            | (N (_, list)::tail2), [] -> let sol1 = tryFindPathtoAux v list [0]
                                          let sol2 = tryFindPathtoAux v tail2 [1]
                                          if sol1 <> None then sol1
                                          elif sol2 <> None then sol2
                                          else None
            | (N (_, list)::tail2), head::tail -> let sol1 = tryFindPathtoAux v list (0::path)
                                                  let sol2 = tryFindPathtoAux v tail2 (head + 1::tail)
                                                  if sol1 <> None then sol1
                                                  elif sol2 <> None then sol2
                                                  else None

    let rec tryFindPathto v t =
        match t with 
            | N (value, []) when value = v -> Some []
            | N (_, list) -> tryFindPathtoAux v list []

    [<EntryPoint>]
    let main argv =


        printfn "Get from Joe -> %A" (get("Joe", sb))
        printfn "Cunt A leaves in tree -> %A" (countOcurrences tree3T)
        printfn "List of values -> %A" (toList ta)
        printfn "Is [] a path of ta -> %A" (isPath [] ta)
        printfn "Is [1; 1; 0] a path of ta -> %A" (isPath [1; 1; 0] ta)
        printfn "Is [6; 7; 8] a path of ta-> %A" (isPath [6; 7; 8] ta)
        printfn "Try find value g in ta-> %A" (tryFindPathto "g" ta)
        

        0;;