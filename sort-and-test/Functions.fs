namespace SortAndTest

module Functions = 

    let rec merge(list1, list2) =
        match (list1, list2) with 
            | (list1, []) -> list1
            | ([], list2) -> list2
            | (head1::tail1, head2::tail2) when head1 < head2 -> head1::merge(tail1, list2)
            | (list1, head2::tail2) -> head2::merge(list1, tail2)

    let rec reverse(list) = 
        match (list) with 
            | [] -> []
            | head::tail -> reverse(tail) @ [head]
    
    let split(list) =
        
        let rec splitRec(list, n, list1, list2) = 
            match (list, n) with
                | ([], _) -> (list1, list2)
                | (head::tail, n) when n = 0 -> splitRec(tail, 1, head::list1, list2)
                | (head::tail, n) when n = 1 -> splitRec(tail, 0, list1, head::list2)
                | (_, _) -> ([], [])



        let list1, list2 = splitRec(list, 0, [], [])
        reverse(list1), reverse(list2)


    // let rec sort(list) = 
    //     match list with 
    //         | ([]) -> 
    //         | (head::tail) -> split(list)


    let sort(list) = 

       let rec sortRec(list1, list2) = 
            match(list1, list2) with 
                | ([], []) -> []
                | (list1, []) -> list1
                | ([], _) -> list2
                | ([last1], [last2]) -> merge([last1], [last2])
                | (list1, list2) -> merge(sortRec(split(list1)), sortRec(split(list2)))

       sortRec(split(list))


    let ordered(list) = 
         
        let rec orderedRec(list, elem) = 
            match list with 
                | [] -> true
                | head::tail when head >= elem -> orderedRec(tail, head)
                | _ -> false

        orderedRec(list, list.Head)


    let orderedSort(xs: int list) = ordered(sort xs)


    let rec increment(x, counting) = 
        match (counting) with 
            | ([]) -> []
            | ((number, count)::tail) when x = number -> (number, count+1)::tail
            | head::tail -> head::increment(x, tail)


    let rec countNumber(number, list, count) =
        match list with 
            | [] -> (number, count)
            | head::tail when head = number -> countNumber(number, tail, count+1)
            | head::tail -> countNumber(number, tail, count)


    let toCounting(list) = 
        
        let rec toCountingRec(list, number, count) =
            match list with
                | [] -> [(number, count)]
                | head::tail when number = head -> toCountingRec(tail, number, count+1)
                | head::tail -> (number, count)::toCountingRec(tail, head, 1)

        let sortedList = sort(list)
        toCountingRec(sortedList, sortedList.Head, 0)
    


    [<EntryPoint>]
    let main argv =

        let list1 = [1;2;3]
        let list2 = [4;5;6;7;8]
        let list3 = [1;5;7;9]
        let list4 = [2;4;5;6;10;11]

        let unorderedList = [2;3;1;14;10;3;1;8;4]

        printfn "Aqui va la lista: %A" (merge(list1, list2))
        printfn "Aqui va la segunda lista: %A" (merge(list4, list3))
        printfn "Split [1;3;5;6;8;10;14;15;20] -> %A" (split([1;3;5;6;8;10;14;15;20]))
        printfn "Order list [2;3;1;14;10;3;1;8;4] -> %A" (sort(unorderedList))
        printfn "Is [2;4;5;6;10;11] an ordered list? -> %A" (ordered([2;4;5;6;10;11]))
        printfn "Is [2;3;1;14;10;3;1;8;4] an ordered list? -> %A" (ordered([2;3;1;14;10;3;1;8;4]))
        printfn "Count the following list: [3;2;6;3;2;1] -> %A" (toCounting([3;2;6;3;2;1]))
        printfn "Increment the number of 3 in [(1,1);(2,2);(3,2);(6,1)] -> %A" (increment(3, [(1,1);(2,2);(3,2);(6,1)]))
        0;;

                




