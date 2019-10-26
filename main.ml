
(** Returns net activation (scalar) for a single unit using our
list-based input and weight representation and Eqn (1) *)
(* Signature: val netUnit : float list * float list -> float = <fun> *)
let netUnit (inputs, weights) = List.hd inputs;;

(* Returns net activation computation for entire network
as a vector (list) of individual unit activations *)
(* Signature: val netAll : float list * float list list -> float list = <fun> *)
let netAll (state, weightMatrix) = 5.0;;

(** Returns ’squashed’ unit output.
Implements Hopfield activation function
corresponding to Eqn (3) for single (-1,1) unit *)
(* Signature: val hop11Activation : float * float -> float = <fun> *)
let hop1Activations (net, oldo) = 6.0;;

(** Returns next state vector  *)
(* Signature: val nextState : float list * float list list -> float list = <fun> *)
let nextState (currentState, weightMatrix) = 7.0;;

(** Returns network state after n time steps; i.e.,  update of N time steps *)
(* Signature: val updateN : float list * float list list * int -> float list = <fun> *)
let updateN (currentState, weightMatrix, n) = 8.0;;

(** Returns weight matrix for only one stored state,
used as a ’warmup’ for the next function *)
(* Signature: val hopTrainAstate : float list -> float list list = <fun> *)
let hopTrainAstate (astate) = 9.0;;

(** This returns weight matrix, given a list of stored states
(shown previously) using Eqns (4) and (5) *)
(* Signature: val hopTrain : float list list -> float list list = <fun> *)
let hopTrain (allStates) = 10.00;;

let c = netUnit([88.0;1.0],[1.0;2.0]);;
Printf.printf "Debug: %f\n" c;
