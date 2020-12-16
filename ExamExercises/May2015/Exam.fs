namespace May2015

module Exam = 

    // 1.1
    let rec repeat s n = 
        if n = 0 then ""
        else s + repeat s (n-1)

    // 1.2
    let rec f s1 s2 n =
        if n = 0 then ""
        elif n % 2 = 0 then s1 + "\n" + s2 + "\n" + f s1 s2 (n-2)
        else s1 + "\n" + f s1 s2 (n-1)

    // 1.3
    let rec vizAux m string = 
        if m = 0 then "\n"
        else string + vizAux (m-1) string
    
    let rec viz m n =
        let rec viz2 m n word =
            match n with
                | n when n = 0 -> ""
                | n when word = "XO" -> vizAux m word + viz2 m (n-1) "OX"
                | _ -> vizAux m word + viz2 m (n-1) "XO"
        
        viz2 m n "XO"

    // 1.4

    // Tail recursive accumulating parameter
    let rec repeatAP s n result =
        if n = 0 then result
        else repeatAP s (n-1) (result+s)
    

    // Tail recursive continuation-based ??? Not done well
    let rec repeatCB s n =
        let mutable result = ""
        for i in 0 .. n do
            result <- result + s
        result


    // 2.1
    let rec mixMap f list1 list2 =
       match list1, list2 with 
            | head1::tail1, head2::tail2 -> (f head1, head2)::mixMap f tail1 tail2
            | _ -> []

    // 2.2
    let unmixMap2 f g list =
       let rec unmixMapRec f g list result1 result2 = 
           match list with 
                | (x, y)::tail -> unmixMapRec f g tail ((f x)::result1) ((g y)::result2)
                | _ -> List.rev result1, List.rev result2

       unmixMapRec f g list [] []

    
    // 3
    type Tree<'a> = Lf | Br of Tree<'a> * 'a * Tree<'a>

    let t = Br(Br(Br(Lf,1,Lf), 2, Br(Lf,3,Lf)), 4, Br(Br(Lf,5,Lf), 6, Br(Lf,7,Lf)))

    // 3.1
    let rec reflect t =
        match t with
            | Lf -> Lf
            | Br (tree1, value, tree2) -> Br (reflect tree2, value, reflect tree1)

    // 3.2
    let rec getValue t =
        match t with
            | Lf -> 0
            | Br (tree1, value, tree2) -> getValue tree1 + value + getValue tree2 

    let accumulate t =
        let rec accumulateAux t acc =
            match t with
                | Lf -> Lf
                | Br (tree1, value, tree2) -> Br (accumulateAux tree1 (value+acc), (value+acc), accumulateAux tree2 (value + acc + getValue tree1))

        accumulateAux t 0
    
    // 3.3 types & stuff I don't need

    // 4
    type CourseNo = int
    type Title = string
    type ECTS = int
    type CourseDesc = Title * ECTS

    type CourseBase = Map<CourseNo, CourseDesc>

    // 4.1
    let rec isValidCourseDesc desc =
        let (_, ects) = desc 
        ects % 5 = 0 && ects >=5

    // 4.2
    let rec isValidCourseBase (cb : CourseBase) : bool= 
        Map.forall (fun _ description -> isValidCourseDesc description) cb

    type Mandatory = Set<CourseNo>
    type Optional = Set<CourseNo>
    type CourseGroup = Mandatory * Optional

    // 4.3
    let disjoint s1 s2 = Set.intersect s1 s2 |> Set.isEmpty

    // 4.4 
    let sumECTS (cs : Set<CourseNo>) (cb : CourseBase) : int =
        let found = Map.filter (fun cn cd -> Set.contains cn cs) cb
        Map.fold (fun acc courseNumber (title, ects) -> acc+ects) 0 found


    // 4.5
    let rec isValidCourseGroup (man, opt) courseBase =
        let cond1 = disjoint man opt
        let cond2 = sumECTS man courseBase <= 45
        let cond3 = sumECTS man courseBase = 45 && Set.isEmpty opt
        let cond4 = (sumECTS man courseBase + sumECTS opt courseBase) >= 45
        cond1 && (cond2 || cond3) && cond4

    type BasicNaturalScience = CourseGroup
    type TechnologicalCore = CourseGroup
    type ProjectProfessionalSkill = CourseGroup
    type Elective = CourseNo -> bool 

    type FlagModel = BasicNaturalScience * TechnologicalCore * ProjectProfessionalSkill * Elective

    type CoursePlan = Set<CourseNo>

    // 4.6
    let isValidAuxDisjoint (mandatory1, optional1) (mandatory2, optional2) = 
        disjoint (Set.union mandatory1 optional1) (Set.union mandatory2 optional2)

    let isValidAuxElective (mandatory, optional) elective = 
        //Set.forall (fun courseNumber -> elective(courseNumber)) (mandatory + optional)
        Set.forall elective (mandatory + optional)


    let isValid (bns, tc, pps, ep) cb =
        (isValidCourseGroup bns cb) && (isValidCourseGroup tc cb) && (isValidCourseGroup pps cb) &&
        (isValidAuxDisjoint bns tc) && (isValidAuxDisjoint bns pps) && (isValidAuxDisjoint tc pps) &&
        (isValidAuxElective bns ep) && (isValidAuxElective tc ep) && (isValidAuxElective pps ep)

    // 4.7
    let CheckPlanAux cs (man1, op1) (man2, op2) (man3, op3) ep cb =
        let is1 = Set.intersect cs (man1 + op1)
        let is2 = Set.intersect (cs - is1) (man2 + op2)
        let is3 = Set.intersect (cs - is1 - is2) (man3 + op3)
        let cond4 = sumECTS ((cs - is1 - is2 - is3)) cb = 45
        sumECTS is1 cb = 45 && sumECTS is2 cb = 45 && sumECTS is3 cb = 45 && cond4

    let checkPlan cs (bns, tc, pps, ep) cb =
        sumECTS cs cb = 180 && CheckPlanAux cs bns tc pps ep cb


    [<EntryPoint>]
    let main argv =


        printfn "Repeat 'ab' 3 times -> %s" (repeat "ab" 3)
        printfn "Repeat 'ab' 0 times -> %s" (repeat "ab" 0)

        printfn "Repeat f function 'ab' 'cd' 4 times -> %s" (f "ab" "cd" 4)
        printfn "Repeat f function 'XO' 'OX' 3 times -> %s" (f "XO" "OX" 3)

        printfn "Viz 4 5 -> %s" (viz 4 5)
        
        printfn "RepeatCB 'ab' 'cd' 4 times -> %s" (repeatCB "ab" 3)

        printfn "Reflect t -> %A" (reflect t)
        printfn "Accumulate t -> %A" (accumulate t)
        
        0;;