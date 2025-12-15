open System

let evt = Event<int>()

// Create & start a worker that triggers the event with
// a random number every second (runs in background)
let worker = async {
  let rnd = Random()
  while true do
    evt.Trigger(rnd.Next(10))
    do! Async.Sleep(1000) }

Async.Start(worker)    

// Derived event that sums all produced even numbers 
// (and triggers each time the sum changes)
let sums = 
  evt.Publish
  |> Event.filter (fun n -> n % 2 = 0)
  |> Event.scan (fun st n -> st + n) 0

// Create a handler that can be added and removed
// (you can call sums.Add(fun n -> ...) but this 
// cannot be later removed)
let handler = Handler<int>(fun _ n ->
  printfn "%d" n)

sums.AddHandler(handler)
sums.RemoveHandler(handler)
  

