open System

let doFunc() =
    //getPerfectSquareNum takes one int argument and returns int value
    let getPerfectSquareNum (x: int) : int = pown x 2 
    printfn "x^2 = %i" (getPerfectSquareNum(10))
 
doFunc()

Console.ReadKey() |> ignore