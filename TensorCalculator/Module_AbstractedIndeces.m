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


Get["Module_Matrix.m"];


(* ::Input:: *)
(*(* Indeces As Variables: *)*)
(*(* The order is "{Lower indeces, Upper indeces}", or "{Co-variant indeces, Contra-variant indeces}" if you like. *)*)


metric[{{i1_,i2_},{}}] := metricMatrix[[i1,i2]];
metric[{{},{i1_,i2_}}] := metricUpIndexMatrix[[i1,i2]];
affineConnection[{{i1_,i2_,i3_},{}}] := gammaDownIndexMatrix[[i1,i2,i3]];
affineConnection[{{i2_,i3_},{i1_}}] := affineConnectionMatrix[[i1,i2,i3]];
(* To be continued. *)


(* ::Input:: *)
(*(* Define Co-Variant Derivative: *)*)
(*(* Denoted by "CoD". *)*)


(* $A^{\vec{j1}}_{\vec{j2}}_{;j3}$: *)
(* Variales in CoD[]:
	"Indeces" is the indeces of the "A" as a list: "{Lower indeces, Upper indeces}";
	"j3" is the coordiante index by which the co-variant derivative is taken. *)

CoD[A_, Indeces_, j3_] := Module[{LowerRank, UpperRank, CoVariantDerivate, UpperIndeces, LowerIndeces,j1,j2},
	UpperIndeces = Indeces[[2]];
	LowerIndeces = Indeces[[1]];
	UpperRank = Length[UpperIndeces];
	LowerRank = Length[LowerIndeces];
	CoVariantDerivate = Simplify[D[A[{LowerIndeces,UpperIndeces}], coordinates[[j3]]] + (1-KroneckerDelta[UpperRank,0])*Sum[Sum[affineConnection[{{i1,j3},{UpperIndeces[[j1]]}}]*A[{LowerIndeces, ReplacePart[UpperIndeces,j1->i1]}],{i1,1,dim}], {j1,1,UpperRank}] - (1-KroneckerDelta[LowerRank,0])*Sum[Sum[affineConnection[{{LowerIndeces[[j2]],j3},{i1}}]*A[{ReplacePart[LowerIndeces,j2->i1], UpperIndeces}],{i1,1,dim}], {j2,1,LowerRank}]];
	Return[CoVariantDerivate];
]
