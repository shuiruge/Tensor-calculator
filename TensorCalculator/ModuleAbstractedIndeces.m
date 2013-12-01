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
ricci[{{i1_,lo}, {i2_,lo}}] := ricciTensorMatrix[[i1, i2]];
ricci[NoIndex] = ricciScalarMatrix;

einsteinTensor[{{i1_,lo}, {i2_,lo}}] := einsteinTensorMatrix[[i1,i2]]
einsteinTensor[{{i1_,up}, {i2_,up}}] := einsteinTensorUpIndexMatrix[[i1,i2]];
(* To be continued. *)


<<TensorCalculator`Modules`CoVariantDerivative`;
<<TensorCalculator`Modules`Contract`;
<<TensorCalculator`Modules`GenerateMatrix`;


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
