(* ::Package:: *)

(* ::Input:: *)
(*(* Conver an "abstracted-indeces quantity" to a "matrix quantity": *)*)
(*(* Denoted by "generateMatrix[]". *)*)


(* This is a main programme which convert an "abstracted-indeces quantity" "A" (the name, or say 'Head', of the function "A[some indeces]"),
	with its indeces "Indeces", to a pure Mathematica type matrix, which has the same order of indeces as the input "Indeces". *)

generateMatrix[A_, Indeces_] := Module[{result, RecursionTimes},
	RecursionTimes = Length[Indeces];
	result = generateMatrixRecursion[A, Indeces, RecursionTimes];
	Return@result;
]


(* This is a "SICP"-type recursion programme as a piece of programme contained in the main "generateMatrix[]". *)
(* Generate a matrix associated with the "abstracted indeces quantity" "A" (the name, or say 'Head', of the function "A[Indeces]").
	The "RecursionTimes" is the times need for recursion. *)

generateMatrixRecursion[A_, Indeces_, RecursionTimes_] := Module[{result, i1, IndecesReplaced},
	IndecesReplaced = ReplacePart[Indeces, RecursionTimes -> {i1, Indeces[[RecursionTimes,2]]}];
	If[RecursionTimes == 1,
		result = Table[A[IndecesReplaced],{i1,1,dim}],
		result = Table[
					generateMatrixRecursion[A, IndecesReplaced, RecursionTimes-1],
				{i1,1,dim}]
	];
	Return@result
]

