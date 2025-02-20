#time "on"
#r "nuget: Akka.FSharp"
#r "nuget: Akka.TestKit"
open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open System.Collections.Generic

type ProjTwo =
    |Initailize of IActorRef[]
    |AlgoGossip of String
    |Rumor of String
    |AlgoPushSum of Double
    |PushSumCalc of Double * Double * Double
    |PushSumConv of Double * Double
    |TimeCount of int64
    |NodeCount of int

let sw = System.Diagnostics.Stopwatch()

let mutable numNodes = int(string (fsi.CommandLineArgs.GetValue 1))
let topology = string (fsi.CommandLineArgs.GetValue 2)
let algorithm = string (fsi.CommandLineArgs.GetValue 3)

type ActorCount() =
    inherit Actor()
    let mutable msgReceived = 0
    let mutable startTime = 0L
    let mutable numOfNodes =0

    override x.OnReceive(rmsg) = 
        match rmsg :?>ProjTwo with 
        | Rumor message ->
            let endTime = sw.ElapsedMilliseconds
            msgReceived <- msgReceived + 1
            if msgReceived = numOfNodes then
                sw.Stop()
                printfn "Time for convergence: %i ms starttime = %i endtime=%i" (endTime-startTime) (startTime) (endTime)
                sw.Reset()
                Environment.Exit(0)  
           
        | PushSumConv (sumValue,weightValue) ->
            let endTime = sw.ElapsedMilliseconds
            printfn "Sum = %f Weight= %f Average=%f" sumValue weightValue (sumValue/weightValue) 
            sw.Stop()
            printfn "Time for convergence: %i ms" (endTime-startTime)
            sw.Reset()
            Environment.Exit(0)
        | TimeCount strtTime ->
            startTime <-strtTime

        | NodeCount numofnodes ->
            numOfNodes <-numofnodes

        | _->()
 
type Node(actorCount: IActorRef, numResend: int, nodeNum: int)=
    inherit Actor()
    let mutable numMsgHeard = 0 
    let mutable  nghbrs:IActorRef[]=[||]
    
    let mutable sumCurr= nodeNum |> float
    let mutable weight = 1.0
    let mutable termRound = 1
 
    override x.OnReceive(num)=
         
         match num :?>ProjTwo with 
         | Initailize aref->
                nghbrs<-aref

         | AlgoGossip msg ->
                numMsgHeard<- numMsgHeard+1
                
                if(numMsgHeard=10) then 
                      actorCount <! Rumor(msg)
                
                if(numMsgHeard <= numNodes * 10) then
                        //to get randomly select next node, we use Random().Next(minValue, maxValue) 
                        let index= System.Random().Next(0,nghbrs.Length)
                        nghbrs.[index] <! AlgoGossip(msg)

         | AlgoPushSum delta -> 
                        let index= System.Random().Next(0,nghbrs.Length)
                        sumCurr<- sumCurr/2.0
                        weight <-weight/2.0
                        nghbrs.[index] <! PushSumCalc(sumCurr,weight,delta)

         | PushSumCalc (sValue:float,wValue,delta) -> 
                          let  newsum = sumCurr+sValue
                          let newweight = weight + wValue
                          let cal = sumCurr/weight - newsum/newweight |> abs
                          if(cal >delta) then
                            termRound<- 0
                            sumCurr <- sumCurr+sValue
                            weight <- weight + wValue
                            sumCurr <- sumCurr/2.0
                            weight <- weight/2.0
                            let index= System.Random().Next(0,nghbrs.Length)
                            nghbrs.[index] <! PushSumCalc(sumCurr,weight,delta)
                           elif (termRound>=3) then
                             actorCount<! PushSumConv(sumCurr,weight)
                           else
                               sumCurr<- sumCurr/2.0
                               weight <- weight/2.0
                               termRound<- termRound+1
                               let index= System.Random().Next(0,nghbrs.Length)
                               nghbrs.[index] <! PushSumCalc(sumCurr,weight,delta)
         | _-> ()


//get input args
// let mutable numNodes = int(string (fsi.CommandLineArgs.GetValue 1))
// let topology = string (fsi.CommandLineArgs.GetValue 2)
// let algorithm = string (fsi.CommandLineArgs.GetValue 3)

let system = ActorSystem.Create("System")
let mutable actualNumOfNodes=float(numNodes)


numNodes = if topology="2D" || topology="imp2D" then 
                 int(floor((actualNumOfNodes ** 0.5) ** 2.0))
             else
                 numNodes
         
let actorCount=system.ActorOf(Props.Create(typeof<ActorCount>),"actorCount")

match topology  with 
      | "full"->
          let buildTopology = Array.zeroCreate (numNodes+1)
          for i in [0..numNodes] do
              buildTopology.[i]<-system.ActorOf(Props.Create(typeof<Node>,actorCount,10,i+1),"projTwo"+string(i))
          for i in [0..numNodes] do
              buildTopology.[i]<!Initailize(buildTopology)
              
          let boss = System.Random().Next(0,numNodes)
          if algorithm="gossip" then
            actorCount<!NodeCount(numNodes)
            sw.Start()
            actorCount<!TimeCount(sw.ElapsedMilliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]<!AlgoGossip("Gossip Algorithm for Full Topology")
          else if algorithm="push-sum" then
            sw.Start()
            actorCount<!TimeCount(sw.ElapsedMilliseconds)
            printfn "Push sum algorithm for Full"
            buildTopology.[boss]<!AlgoPushSum(10.0 ** -10.0)
      |"line"->
          let buildTopology= Array.zeroCreate (numNodes+1)
          for i in [0..numNodes] do
              buildTopology.[i]<-system.ActorOf(Props.Create(typeof<Node>,actorCount,10,i+1),"ProjTwo"+string(i))
          for i in [0..numNodes] do
              let nghbrArray=[|buildTopology.[((i-1+numNodes)%(numNodes+1))];buildTopology.[((i+1+numNodes)%(numNodes+1))]|]
              buildTopology.[i]<!Initailize(nghbrArray)
          let boss = System.Random().Next(0,numNodes)
          if algorithm="gossip" then
            actorCount<!NodeCount(numNodes)
            sw.Start()
            actorCount<!TimeCount(sw.ElapsedMilliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]<!AlgoGossip("Gossip Algorithm for Line Topology")
          else if algorithm="push-sum" then
            sw.Start()
            actorCount<!TimeCount(sw.ElapsedMilliseconds)
            printfn "Push sum algorithm for Line"
            buildTopology.[boss]<!AlgoPushSum(10.0 ** -10.0)

      |"2D"->
           let gridSize=int(ceil(sqrt actualNumOfNodes))
           let gridCount=gridSize*gridSize
           let buildTopology= Array.zeroCreate (gridCount)
           for i in [0..(gridSize*gridSize-1)] do
              buildTopology.[i]<-system.ActorOf(Props.Create(typeof<Node>,actorCount,10,i+1),"ProjTwo"+string(i))
           
           for i in [0..gridSize-1] do
               for j in [0..gridSize-1] do
                    let mutable nghbrs:IActorRef[]=[||]
                    if j+1<gridSize then
                        nghbrs<-(Array.append nghbrs [|buildTopology.[i*gridSize+j+1]|])
                    if j-1>=0 then
                        nghbrs<-Array.append nghbrs [|buildTopology.[i*gridSize+j-1]|]
                    if i-1>=0 then
                        nghbrs<-Array.append nghbrs [|buildTopology.[(i-1)*gridSize+j]|]
                    if i+1<gridSize then
                        nghbrs<-(Array.append nghbrs [|buildTopology.[(i+1)*gridSize+j]|])
                    
                    printfn "i %i, gridsize %i, negbr length %i" (i) (gridSize) (nghbrs.Length)
                    buildTopology.[i*gridSize+j]<!Initailize(nghbrs)

       
               
           let boss = System.Random().Next(0,gridCount-1)  
           if algorithm="gossip" then
            actorCount<!NodeCount(gridCount-1)
            sw.Start()
            actorCount<!TimeCount(sw.ElapsedMilliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]<!AlgoGossip("Gossip Algorithm for 2D Topology")

      
           else if algorithm="push-sum" then
            sw.Start()
            actorCount<!TimeCount(sw.ElapsedMilliseconds)
            printfn "Push sum algorithm for 2D"
            buildTopology.[boss]<!AlgoPushSum(10.0 ** -10.0)

      |"Imp2D"->
           let gridSize=int(ceil(sqrt actualNumOfNodes))
           let gridCount=gridSize*gridSize
           let buildTopology= Array.zeroCreate (gridCount + 1 )
           for i in [0..(gridSize*gridSize)] do
              buildTopology.[i]<-system.ActorOf(Props.Create(typeof<Node>,actorCount,10,i+1),"ProjTwo"+string(i))
           
           for i in [0..gridSize-1] do
               for j in [0..gridSize-1] do
                    let mutable nghbrs:IActorRef[]=[||]
                    
                    if j+1<gridSize then
                        nghbrs<-(Array.append nghbrs [|buildTopology.[i*gridSize+j+1]|])
                    if j-1>=0 then
                        nghbrs<-Array.append nghbrs [|buildTopology.[i*gridSize+j-1]|]
                    if i-1>=0 then
                        nghbrs<-Array.append nghbrs [|buildTopology.[(i-1)*gridSize+j]|]
                    if i+1<gridSize then
                        nghbrs<-(Array.append nghbrs [|buildTopology.[(i+1)*gridSize+j]|])
                                          
                    nghbrs<-(Array.append nghbrs [|buildTopology.[System.Random().Next(0,gridCount-1) ]|])
                    
                    buildTopology.[i*gridSize+j]<!Initailize(nghbrs)

       
              
           let boss = System.Random().Next(0,gridCount-1)  
           if algorithm="gossip" then
            actorCount<!NodeCount(gridCount-1)
            sw.Start()
            actorCount<!TimeCount(sw.ElapsedMilliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]<!AlgoGossip("Gossip Algorithm for Imperfect 2D Topology")

      
           else if algorithm="push-sum" then
            sw.Start()
            actorCount<!TimeCount(sw.ElapsedMilliseconds)
            printfn "Push sum algorithm for 2D"
            buildTopology.[boss]<!AlgoPushSum(10.0 ** -10.0)

      | _-> ()
      
System.Console.ReadLine()|>ignore

       