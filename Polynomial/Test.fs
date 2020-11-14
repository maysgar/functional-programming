namespace Polynomial

open Polynomial.Partwo
 

module Test = 

    let addPolTest = 
        printfn "Simple add polynomial %A + %A = %A" (addPol([1;2;3], [4;5;6;7;8]))


 
    let AllPolynomialTests =
        addPolTest