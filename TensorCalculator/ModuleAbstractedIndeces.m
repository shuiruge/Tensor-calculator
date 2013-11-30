(* ::Package:: *)

(* ::Input:: *)
(*(* One day, suddenly, I thought that it is the indeces that should be the*)
(*variables when we make tensor analysis, rather than coordinates!*)
(**)
(*For example, for a contraction $A^{ab} B_{b}$, it can be replaced*)
(*equivalently to be, $A^{ac} B_{c}$. You see, the replacement done here is*)
(*the replacement of indeces of the tensor, rather than the coordinates*)
(*contained within the functional form of $A$, of which is the thing that*)
(*we ususally do when doing functional operations!*)
(**)
(*This is a hint that makes me think about this aspect. *)*)


(* Set "up" to denote the upper index, and "lo" the lower index of a tensor. *)

Protect[up,lo];
Protect[NoIndex];


<<TensorCalculator`ModuleMatrix`;


(* ::Input:: *)
(*(* Indeces As Variables: *)*)
(*(* The order is "{LowerIndeces, UpperIndeces}", or "{Co-variant indeces, Contra-variant indeces}" if you like. *)*)


metric[{{i1_,lo}, {i2_,lo}}] := metricMatrix[[i1,i2]];
metric[{{i1_,up}, {i2_,up}}] := metricUpIndexMatrix[[i1,i2]];
affineConnection[{{i1_,lo}, {i2_,lo}, {i3_,lo}}] := gammaDownIndexMatrix[[i1,i2,i3]];
affineConnection[{{i1_,up}, {i2_,lo}, {i3_,lo}}] := affineConnectionMatrix[[i1,i2,i3]];

ricci[{{i1_,lo}, {i2_,lo}, {i3_,lo}, {i4_,lo}}] := rDownIndexMatrix[[i1, i2, i3, i4]];
ricci[{{i1_,up}, {i2_,lo}, {i3_,lo}, {i4_,lo}}] := rUpIndexMatrix[[i1, i2, i3, i4]];
ricci[{{i1_,lo}, {i2_,lo}}] := ricciTensor[[i1, i2]];
ricci[NoIndex] = ricciScalar;

einsteinTensor[{{i1_,lo}, {i2_,lo}}] := einsteinTensorMatrix[[i1,i2]]
einsteinTensor[{{i1_,up}, {i2_,up}}] := einsteinTensorUpIndexMatrix[[i1,i2]];
(* To be continued. *)


(* ::Input:: *)
(*(* Define Co-Variant Derivative: *)*)
(*(* Denoted by "CoD". *)*)


(* $A^{\vec{j1}}_{\vec{j2}}_{;j3}$: *)
(* Variales in CoD[]:
	"Indeces" is the indeces of the "A" as a list: "{Lower indeces, Upper indeces}";
	"j3" is the coordiante index by which the co-variant derivative is taken. *)

CoD[A_, Indeces_, j3_] := Module[{CoVariantDerivate, i1, j2},
	CoVariantDerivate = D[A[Indeces], coordinates[[j3]]];
	If[Indeces != NoIndex,
		i1 = 1;
		While[i1 <= Length[Indeces],
			Which[Indeces[[i1,2]] == up,
				CoVariantDerivate = CoVariantDerivate + Sum[affineConnection[{Indeces[[i1]], {j2,lo}, {j3,lo}}]*A[ReplacePart[Indeces, i1->{j2,up}]], {j2,1,dim}],
				Indeces[[i1,2]] == lo,
				CoVariantDerivate = CoVariantDerivate - Sum[affineConnection[{{j2,up}, Indeces[[i1]], {j3,lo}}]*A[ReplacePart[Indeces, i1->{j2,lo}]], {j2,1,dim}]
			];
			i1++;
		];
	];
	Return[Simplify[CoVariantDerivate]];
]


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


(* ::Input:: *)
(*(* Conver an "abstracted-indeces quantity" to a "matrix quantity": *)*)
(*(* Denoted by "generateMatrix[]". *)*)


(* This is a main programme which convert an "abstracted-indeces quantity" "A" (the name, or say 'Head', of the function "A[some indeces]"),
	with its indeces "Indeces", to a pure Mathematica type matrix, which has the same order of indeces as the input "Indeces". *)

generateMatrix[A_, Indeces_] := Module[{result, RecursionTimes},
	RecursionTimes = Length[Indeces];
	result = generateMatrixSub[A, Indeces, RecursionTimes];
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



(* ::Input:: *)
(*(* --------------------- Some useful small functions ------------------------ *)*)


(* Get the common indeces of two indeces "listA" and "listB". *)

commonIndeces[listA_, listB_] := Module[{result},
	result = Commonest[Join[listA, listB]];
	Return[result];
]


(* Drop the elements appeared in the listB from the listA. *)

dropElements[listA_, listB_] := Module[{resultA, i1},
	i1 = 1;
	While[i1<=Length[listB],
			listA = Delete[listA, Position[listA, listB[[i1]]]];
			i1++;
	];
]
