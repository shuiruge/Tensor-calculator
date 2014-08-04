(* ::Package:: *)


(* load the ModuleMatrix_*.m packages: *)

<<TensorCalculator`Modules`ModuleMatrixWithSimplify`
<<TensorCalculator`Modules`ModuleMatrixWithoutSimplify`

(* Protect the "\[Epsilon]1", which, SPECIALLY, denotes the order of the perturbation. *)

Protect[\[Epsilon]1];


(* (g^ab): *)

metricUpIndexMatrixModule[metricMatrix0_, order0_:order, simplifyQ:_simplifyOrNot] :=
If[simplifyQ == True,
	metricUpIndexMatrixModuleSimplify[metricMatrix0, order0],
	metricUpIndexMatrixModuleWithoutSimplify[metricMatrix0, order0]]


(* Subscript[\[CapitalGamma], abc]: *)

gammaDownIndexMatrixModule[metricMatrix0_, order0_:order, simplifyQ_:simplifyOrNot] :=
If[simplifyQ == True,
	gammaDownIndexMatrixModuleSimplify[metricMatrix0, order0],
	gammaDownIndexMatrixModuleWithoutSimplify[metricMatrix0, order0]]


(* Subscript[\[CapitalGamma]^a, bc], i.e., affine connection: *)

affineConnectionMatrixModule[metricMatrix0_, order0_:order, simplifyQ_:simplifyOrNot] := 
If[simplifyQ == True,
	affineConnectionMatrixModuleSimplify[metricMatrix0, order0],
	affineConnectionMatrixModuleWithoutSimplify[metricMatrix0, order0]]


(* Subscript[R^a, bcd]: *)

rUpIndexMatrixModule[metricMatrix0_, order0_:order, simplifyQ_:simplifyOrNot] := 
If[simplifyQ == True,
	rUpIndexMatrixModuleSimplify[metricMatrix0, order0],
	rUpIndexMatrixModuleWithoutSimplify[metricMatrix0, order0]]


(* Subscript[R, abcd], if you use it. (Or, you can make it to be just \
a comment, if you will not use it at all!): *)

rDownIndexMatrixModule[metricMatrix0_, order0_:order, simplifyQ_:simplifyOrNot] := 
If[simplifyQ == True,
	rDownIndexMatrixModuleSimplify[metricMatrix0, order0],
	rDownIndexMatrixModuleWithoutSimplify[metricMatrix0, order0]]


(* Subscript[R, ab], i.e., Ricci tensor: *)

ricciTensorMatrixModule[metricMatrix0_, order0_:order, simplifyQ_:simplifyOrNot] := 
If[simplifyQ == True,
	ricciTensorMatrixModuleSimplify[metricMatrix0, order0],
	ricciTensorMatrixModuleWithoutSimplify[metricMatrix0, order0]]


(* R, i.e., Ricci scalar: *)

ricciScalarMatrixModule[metricMatrix0_, order0_:order, simplifyQ_:simplifyOrNot] :=
If[simplifyQ == True,
	ricciScalarMatrixModuleSimplify[metricMatrix0, order0],
	ricciScalarMatrixModuleWithoutSimplify[metricMatrix0, order0]]


(* Subscript[G, ab], i.e., Einstein tensor: *)

einsteinTensorMatrixModule[metricMatrix0_, order0_:order, simplifyQ_:simplifyOrNot] :=
If[simplifyQ == True,
	einsteinTensorMatrixModuleSimplify[metricMatrix0, order0],
	einsteinTensorMatrixModuleWithoutSimplify[metricMatrix0, order0]]


(* (G^ab): *)

einsteinTensorUpIndexMatrixModule[metricMatrix0_, order0_:order, simplifyQ_:simplifyOrNot] :=
If[simplifyQ == True,
	einsteinTensorUpIndexMatrixModuleSimplify[metricMatrix0, order0],
	einsteinTensorUpIndexMatrixModuleWithoutSimplify[metricMatrix0, order0]]
