------------------------------ MODULE detective----------------------------- 
EXTENDS Naturals

CONSTANTS my-colour my-position  
(***************************************************************************)
(* We next declare the specification's variables.                          *)
(***************************************************************************)
VARIABLES colour      \* The detectives colour
          position    \* The actual position on the board


TypeOK == /\ colour \in {"yellow" "red" "green" "black" "blue"}
          /\ position in 1..199 

(***************************************************************************)
(* Now we define of the initial predicate, that specifies the initial      *)
(* values of the variables.  I like to name this predicate Init, but the   *)
(* name doesn't matter.                                                    *)
(***************************************************************************)
Init == /\ colour = my-colour
        /\ position = my-position


(***************************************************************************)
(* Actions                                                                 *)
(***************************************************************************)

Move(p,t) == /\ p \in Neighbour(position,t) 
             /\ position' = p
          

Next ==  \/ \E FillSmallJug 
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
