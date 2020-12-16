namespace Polynomial

module Partwo = 
    type Poly = int list
    type Degree
    val addPol : Poly -> Poly -> Poly 
    val mulC : Poly -> Poly -> Poly
    val sub : Poly -> Poly -> Poly
    val mulX : Poly -> Poly
    val mul : Poly -> Poly -> Poly
    val eval : int -> Poly -> Poly
    val deg : Poly -> Degree
    val addD : Degree -> Degree -> Degree
    val isLegal : Poly -> bool
    val reverse : Poly -> Poly 
    val auxPrune : Poly -> Poly 
    val isPrune : Poly -> Poly 
    val toString : Poly -> string 
    val derivative : Poly -> Poly 
    val polPow : Poly -> int -> Poly 
    val compose : Poly -> Poly -> Poly 
    val ofList : int list -> Poly
    val toList : Poly -> int list


