open System

let doFunc() =
    //getPerfectSquareNum takes one int argument and returns int value
    let getNextPerfectSquareNum (x: int) : int = Convert.ToInt32(pown (ceil (sqrt (float x))) 2) //pown (ceil (sqrt x)) 2 
    let i = getNextPerfectSquareNum(10) 
    printfn "next square = %i" (i)
 
doFunc()

Console.ReadKey() |> ignore