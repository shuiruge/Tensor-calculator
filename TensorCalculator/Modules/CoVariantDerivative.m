(* ::Package:: *)

(* ::Input:: *)
(*(* Define Co-Variant Derivative: *)*)
(*(* Denoted by "CoD". *)*)


(* $A^{\vec{j1}}_{\vec{j2}}_{;j3}$: *)
(* Variales in CoD[]:
	"Indeces" is the indeces of the "A" as a list: "{Lower indeces, Upper indeces}";
	"j3" is the coordiante index by which the co-variant derivative is taken. *)

CoD[A_, Indeces_, j3_] := Module[{CoVariantDerivate, i1, j2},
	CoVariantDerivate = D[A[Indeces], coordinates[[j3]]];
	If[Length[Indeces] != 0 (* Not a scalar. *),
		For[i1 = 1, i1 <= Length[Indeces], i1++,
			If[Indeces[[i1,2]] == lo,
				CoVariantDerivate = CoVariantDerivate - Sum[affineConnection[{{j2,up}, Indeces[[i1]], {j3,lo}}]*A[ReplacePart[Indeces, i1 -> {j2,lo}]], {j2,1,dim}]
			];
			If[Indeces[[i1,2]] == up,
				CoVariantDerivate = CoVariantDerivate + Sum[affineConnection[{Indeces[[i1]], {j2,lo}, {j3,lo}}]*A[ReplacePart[Indeces, i1 -> {j2,up}]], {j2,1,dim}]
			];
		]
	];
	Return@Simplify[CoVariantDerivate];
]
