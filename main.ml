(* Generates a list of weights determined by num * list[n] *)
let rec helper2 (num, lst, el, n) =
if el == n then
try 0.0::helper2(num, (List.tl lst), el, n+1)
with
| _ -> [0.0]
else
try num *. (List.hd lst)::helper2(num, (List.tl lst), el, n+1)
with
| _ -> try [num *. (List.hd lst)]
with
| _ -> []
;;

(* Generates a list of list of weights *)
let rec helper (num, lst, constLst, el, n) =
try helper2(num, constLst, el, n)::helper(List.hd (List.tl lst), List.tl lst, constLst, el+1, 0)
with
| _ -> try [helper2(num, constLst, el, n)]
with
| _ -> [[5.]]
;;

let rec reduce1 (lst1, lst2) =
try (List.hd lst1 +. List.hd lst2)::reduce1(List.tl lst1, List.tl lst2)
with _ -> try [(List.hd lst1 +. List.hd lst2)]
          with _ -> try [List.hd lst1]
                    with _ -> [0.]
;;
(* returns a summed single list from two separate double lists *)
let rec reduce2 (lst1, lst2) =
try reduce1(List.hd lst1, List.hd lst2)::reduce2(List.tl lst1, List.tl lst2)
with _ -> try [reduce1(List.hd lst1, List.hd lst2)]
          with _ -> [List.hd lst1]
;;

(** Returns net activation (scalar) for a single unit using our
list-based input and weight representation and Eqn (1) *)
(* Signature: val netUnit : float list * float list -> float = <fun> *)
let rec netUnit (inputs, weights) =
try List.hd inputs *. List.hd weights +. netUnit(List.tl inputs, List.tl weights)
with
| _ -> try List.hd inputs *. List.hd weights
with
| _ -> 0.0
;;

(* Returns net activation computation for entire network
as a vector (list) of individual unit activations *)
(* Signature: val netAll : float list * float list list -> float list = <fun> *)
let rec netAll (state, weightMatrix) =
try  netUnit (state, (List.hd weightMatrix))::netAll (state, (List.tl weightMatrix))
with
| _ -> try [netUnit (state, (List.hd weightMatrix))]
with
| _ -> []
;;

(** Returns ’squashed’ unit output.
Implements Hopfield activation function
corresponding to Eqn (3) for single (-1,1) unit *)
(* Signature: val hop11Activation : float * float -> float = <fun> *)
let hop11Activation (net, oldo) =
if net < 0.0 then -1.0
else if net > 0.0 then 1.0
else oldo
;;

(** Returns next state vector  *)
(* Signature: val nextState : float list * float list list -> float list = <fun> *)
let rec nextState (currentState, weightMatrix) =
try  hop11Activation((netUnit(currentState,  (List.hd weightMatrix))), (List.hd currentState))::nextState (currentState, (List.tl weightMatrix))
with
| _ -> try [hop11Activation((netUnit(currentState,  (List.hd weightMatrix))), (List.hd currentState))]
with
| _ -> []
;;

(** Returns network state after n time steps; i.e.,  update of N time steps *)
(* Signature: val updateN : float list * float list list * int -> float list = <fun> *)
let rec updateN (currentState, weightMatrix, n) =
if n > 1 then updateN((nextState (currentState, weightMatrix)), weightMatrix, n-1)
else nextState(currentState, weightMatrix)
;;

(** Returns weight matrix for only one stored state,
used as a ’warmup’ for the next function *)
(* Signature: val hopTrainAstate : float list -> float list list = <fun> *)
(*Need to:*)
let hopTrainAstate (astate) =
try helper((List.hd astate), astate, astate, 0, 0)
with
| _ -> [[]]
;;

(** This returns weight matrix, given a list of stored states
(shown previously) using Eqns (4) and (5) *)
(* Signature: val hopTrain : float list list -> float list list = <fun> *)
let rec hopTrain (allStates) =
try (hopTrainAstate(List.hd allStates)::hopTrain(List.tl allStates))
with _ -> try [hopTrainAstate(List.hd allStates)]
          with _ -> [[[]]]

(* try hopTrain(allStates)
with _ -> [[]] *)
;;

let os1 = [1.0; -1.0; 1.0; -1.0];;
let os2 = [-1.0; -1.0; 1.0; -1.0];;
let os3 = [-1.0; -1.0; 1.0; 1.0];;
let w =
[[0.; -1.; 1.; -1.]; [-1.; 0.; -1.; 1.]; [1.; -1.; 0.; -1.];
[-1.; 1.; -1.; 0.]];;
let we=[[0.0;-1.0];[-1.0;0.0]];;
let oi=[-1.0;-1.0];;
let w2 =
[[0.; 1.; -1.; -1.]; [1.; 0.; -3.; 1.]; [-1.; -3.; 0.; -1.];
[-1.; 1.; -1.; 0.]];;
(* Printf.printf "%f\n%f\n%f\n%f\n" (List.hd c) (List.hd (List.tl c)) (List.hd (List.tl (List.tl c))) (List.hd (List.tl (List.tl (List.tl c)))); *)
