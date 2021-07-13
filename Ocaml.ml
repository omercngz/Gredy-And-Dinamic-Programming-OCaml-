
let optmin x y =
  match x,y with
  | None,a | a,None -> a
  | Some x, Some y-> Some (min x y)

let optsucc = function
  | Some x -> Some (x+1)
  | None -> None

(* DYNAMIC PROGRAMMING *)
let dynamic money amount =
  let rec loop n =
    let onepiece acc piece =
      match n - piece with
      | 0 -> 
             Some 1
      | x -> if x < 0 then
               None
             else
               optmin (optsucc (loop x)) acc
    in

    List.fold_left onepiece None money
  in loop amount
  
  ;;
(*////////////////////////////////////////////////////*)


(* GREEDY ALGORITHM *)
 
let greedy money amount =
  let rec loop given amount =
    if amount = 0 then given
    else 
      let coin = List.find ((>=) amount) money in
      loop (coin::given) (amount - coin)
  in loop [] amount
      ;;
  
