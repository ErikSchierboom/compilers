module List =
  /// Attempts to map all elements of the input list and 
  /// succeeds only if each of the operation succeeds.
  /// If any mapping operation fails, returns 'None'.
  let rec tryMap (f:'a -> 'b option) (xs:'a list) : 'b list option =
    match xs with 
    | [] -> Some []
    | x::xs -> 
        match f x with 
        | None -> None
        | Some x -> 
            match tryMap f xs with 
            | None -> None
            | Some xs -> Some(x::xs)

/// Safe division. Returns 'None' when 'b = 0'
let safeDiv a b = 
  if b = 0 then None else Some(a / b)

// This will work and return 'Some' with the 
// results of dividing 12 by the given numbers
[2;3;4;6]
|> List.tryMap (fun n -> safeDiv 12 n)

// This fails and returns 'None' because the
// last call to 'safeDiv' fails and returns 'None'
[2;3;4;6;0]
|> List.tryMap (fun n -> safeDiv 12 n)
