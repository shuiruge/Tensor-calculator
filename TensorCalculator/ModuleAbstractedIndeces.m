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


<<TensorCalculator`ModuleMatrix`;


(* ::Input:: *)
(*(* Indeces As Variables: *)*)
(*(* The order is "{Lower indeces, Upper indeces}", or "{Co-variant indeces, Contra-variant indeces}" if you like. *)*)


metric[{{i1_,i2_},{}}] := metricMatrix[[i1,i2]];
metric[{{},{i1_,i2_}}] := metricUpIndexMatrix[[i1,i2]];
affineConnection[{{i1_,i2_,i3_},{}}] := gammaDownIndexMatrix[[i1,i2,i3]];
affineConnection[{{i2_,i3_},{i1_}}] := affineConnectionMatrix[[i1,i2,i3]];

ricci[{{i1_, i2_, i3_, i4_}, {}}] := rDownIndexMatrix[[i1, i2, i3, i4]];
ricci[{{i2_, i3_, i4_}, {i1_}}] := rUpIndexMatrix[[i1, i2, i3, i4]];
ricci[{{i1_, i2_}, {}}] := ricciTensor[[i1, i2]];
ricci[{},{}] = ricciScalar;

einsteinTensor[{{i1_,i2_},{}}] := einsteinTensorMatrix[[i1,i2]]
einsteinTensor[{{},{i1_,i2_}}] := einsteinTensorUpIndexMatrix[[i1,i2]];
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


(* ::Input:: *)
(*(* Define the transformation from Abstracted-version to Matrix-verion: *)*)
(*(* Denoted by "transform". *)*)


transform[abstracted_, matrix_] := Module[{result},
	
	return[result];
]


(* ::Input:: *)
(*(* Define contraction of indeces: *)*)
(*(* Denoted by "contract". *)*)


contract[A_, IndecesA_, B_, IndecesB_] := Module[{result, i1},
	commonPart[IndecesA[[1]], IndecesB[[2]]];
	commonPart[IndecesA[[2]], IndecesB[[1]]];
	result = Table[A[IndecesA]*B[IndecesB], {}];
]
