(* ::Package:: *)

(* ::Input:: *)
(*(* Define contraction of indeces: *)*)
(*(* Denoted by "contract[]". *)*)


(* It's a main programme for contration of two quantities "A" and "B", with indeces "IndecesA" and "IndecesB" respectively,
	both of which contain the "lo" and "up" instruction. *)
(* "A" and "B" are the name ('Head', in Mathematica) of the functions; "IndecesA" and "IndecesB" are lists.
	For detials, see the "Documentation.nb", section "Contraction" *)

contract[A_, IndecesA_, B_, IndecesB_] := Module[{result, ContractedIndeces},
	ContractedIndeces = contractedIndeces[IndecesA, IndecesB];
	result = contractRecursion[A, IndecesA, B, IndecesB, ContractedIndeces];
	Return@result;
]


(* Function used in "contract[]". *)
(* It's an "SICP type" recursion programme that return the contraction of quantity "A" with indeces "IndecesA" and "B" with indeces "IndecesB",
	as well as the list, WITHOUT "LO" AND "UP" INSTRUCTIONS, which shows the all the indeces to be contracted,
	which of course is determined completely by "IndecesA" and "IndecesB",
	but being written down here for simplification of programming. *)

contractRecursion[A_, IndecesA_, B_, IndecesB_, ContractedIndeces_] := Module[{result, i1, IndecesAReplaced, IndecesBReplaced},
	IndecesAReplaced = ReplacePart[IndecesA, Position[IndecesA,ContractedIndeces[[1]]][[1]] -> i1];
	IndecesBReplaced = ReplacePart[IndecesB, Position[IndecesB,ContractedIndeces[[1]]][[1]] -> i1];
	If[Length[ContractedIndeces] == 1,
		result = Sum[A[IndecesAReplaced]*B[IndecesBReplaced], {i1,1,dim}],
		result = Sum[
					contractRecursion[A, IndecesAReplaced, B, IndecesBReplaced, Delete[ContractedIndeces,1]],
				{i1,1,dim}]
	];
	Return@result;
]


(* Function used in "contract[]". *)
(* It determines which indeces shoud be contracted, which should not. *)
(* It return the list, which is the common part of two lists of indeces "IndecesA" and "IndecesB", but WITHOUT "LO" AND "UP" INSTRUCTIONS. *)

contractedIndeces[IndecesA_, IndecesB_] := Module[{result = {}, j1 = 1},
	While[j1 <= Length[IndecesB],
		If[MemberQ[IndecesA, IndecesB[[j1,1]], 2] && FreeQ[IndecesB[[j1,1]], _Symbol] == False,
			AppendTo[result, IndecesB[[j1,1]]];
		];
		j1++;
	];
	Return@result
]
