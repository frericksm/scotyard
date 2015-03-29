------------------------------ MODULE strategy ------------------------------- 
EXTENDS Naturals

  
(***************************************************************************)
(* We next declare the specification's variables.                          *)
(***************************************************************************)
VARIABLES mr_x       \* Properties of Mr X 
          detectives \* The set of detecives


TypeOK == /\ mr_x.possible_positions \subset \in 1..199 
          /\ detectives \in [d1 : \in 1..199
                             d2 : \in 1..199
                             d3 : \in 1..199
                             d4 : \in 1..199
                             d5 : \in 1..199]
 


(***************************************************************************)
(* Now we define of the initial predicate, that specifies the initial      *)
(* values of the variables.  I like to name this predicate Init, but the   *)
(* name doesn't matter.                                                    *)
(***************************************************************************)
Init == /\ big = 0 
        /\ small = 0

(***************************************************************************)
(* Now we define the actions that our hero can perform.  There are three   *)
(* things they can do:                                                     *)
(*                                                                         *)
(*   - Pour water from the faucet into a jug.                              *)
(*                                                                         *)
(*   - Pour water from a jug onto the ground.                              *)
(*                                                                         *)
(*   - Pour water from one jug into another                                *)
(*                                                                         *)
(* We now consider the first two.  Since the jugs are not calibrated,      *)
(* partially filling or partially emptying a jug accomplishes nothing.     *)
(* So, the first two possibilities yield the following four possible       *)
(* actions.                                                                *)
(***************************************************************************)
FillSmallJug  == /\ small' = 3 
                 /\ big' = big

FillBigJug    == /\ big' = 5 
                 /\ small' = small

EmptySmallJug == /\ small' = 0 
                 /\ big' = big

EmptyBigJug   == /\ big' = 0 
                 /\ small' = small

(***************************************************************************)
(* We now consider pouring water from one jug into another.  Again, since  *)
(* the jugs are not callibrated, when pouring from jug A to jug B, it      *)
(* makes sense only to either fill B or empty A. And there's no point in   *)
(* emptying A if this will cause B to overflow, since that could be        *)
(* accomplished by the two actions of first filling B and then emptying A. *)
(* So, pouring water from A to B leaves B with the lesser of (i) the water *)
(* contained in both jugs and (ii) the volume of B. To express this        *)
(* mathematically, we first define Min(m,n) to equal the minimum of the    *)
(* numbers m and n.                                                        *)
(***************************************************************************)
Min(m,n) == IF m < n THEN m ELSE n

(***************************************************************************)
(* Now we define the last two pouring actions.  From the observation       *)
(* above, these definitions should be clear.                               *)
(***************************************************************************)
SmallToBig == /\ big'   = Min(big + small, 5)
              /\ small' = small - (big' - big)

BigToSmall == /\ small' = Min(big + small, 3) 
              /\ big'   = big - (small' - small)

(***************************************************************************)
(* We define the next-state relation, which I like to call Next.  A Next   *)
(* step is a step of one of the six actions defined above.  Hence, Next is *)
(* the disjunction of those actions.                                       *)
(***************************************************************************)
Next ==  \/ FillSmallJug 
         \/ FillBigJug    
         \/ EmptySmallJug 
         \/ EmptyBigJug    
         \/ SmallToBig    
         \/ BigToSmall    

(***************************************************************************)
(* We define the formula Spec to be the complete specification, asserting  *)
(* of a behavior that it begins in a state satisfying Init, and that every *)
(* step either satisfies Next or else leaves the pair <<big, small>>       *)
(* unchanged.                                                              *)
(***************************************************************************)
Spec == Init /\ [][Next]_<<big, small>> 
-----------------------------------------------------------------------------

(***************************************************************************)
(* Remember that our heros must measure out 4 gallons of water.            *)
(* Obviously, those 4 gallons must be in the 5 gallon jug.  So, they have  *)
(* solved their problem when they reach a state with big = 4.  So, we      *)
(* define NotSolved to be the predicate asserting that big # 4.            *)
(***************************************************************************)
NotSolved == big # 4

(***************************************************************************)
(* We find a solution by having TLC check if NotSolved is an invariant,    *)
(* which will cause it to print out an "error trace" consisting of a       *)
(* behavior ending in a states where NotSolved is false.  Such a           *)
(* behavior is the desired solution.  (Because TLC uses a breadth-first    *)
(* search, it will find the shortest solution.)                            *)
(***************************************************************************)
=============================================================================
