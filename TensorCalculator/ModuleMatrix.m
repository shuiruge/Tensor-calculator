(* ::Package:: *)

(*
helpMeTensorCalculator = Print["'metricUpIndexMatrixModule[metricMatrix]' for\!\(\*FormBox[SuperscriptBox[\(g\), \(ab\)],
TraditionalForm]\),where g is the metric;\n'gammaDownIndexMatrixModule[metricMatrix]' for\!\(\*FormBox[SubscriptBox[\(\[CapitalGamma]\), \(abc\)],
TraditionalForm]\);\n'affineConnectionModule[metric]' for\!\(\*FormBox[SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(a\)], \(bc\)],
TraditionalForm]\),i.e.,the affine connection;\n'rUpIndexMatrixModule[metricMatrix]' for\!\(\*FormBox[SubscriptBox[SuperscriptBox[\(R\), \(a\)], \(bcd\)],
TraditionalForm]\);\n'rDownIndexModule[metric]' for\!\(\*FormBox[SubscriptBox[\(R\), \(abcd\)],
TraditionalForm]\);\n'ricciTensorMatrixModule[metricMatrix]' for\!\(\*FormBox[SubscriptBox[\(R\), \(ab\)],
TraditionalForm]\),i.e.,the Ricci tensor;\n'ricciScalarMatrixModule[metricMatrix]' for\!\(\*FormBox[\(R\),
TraditionalForm]\),i.e.,the Ricci scalar;\n'einsteinTensorMatrixModule[metricMatrix]' for\!\(\*FormBox[SubscriptBox[\(G\), \(ab\)],
TraditionalForm]\),i.e.,the Einstein tensor;\n'einsteinTensorUpIndexMatrixModule[metricMatrix]' for\!\(\*FormBox[SuperscriptBox[\(G\), \(ab\)],
TraditionalForm]\)."];
*)


(* Protect the "\[Epsilon]1", which, SPECIALLY, denotes the order of the perturbation. *)

Protect[\[Epsilon]1];


(* (g^ab): *)

metricUpIndexMatrixModule[metricMatrix0_, order_:0, simplifyQ_:True] :=
If[simplifyQ == True,
	metricUpIndexMatrixModuleSimplify[metricMatrix0, order],
	metricUpIndexMatrixModuleWithoutSimplify[metricMatrix0, order]]


(* Subscript[\[CapitalGamma], abc]: *)

gammaDownIndexMatrixModule[metricMatrix0_, order_:0, simplifyQ_:True] :=
If[simplifyQ == True,
	gammaDownIndexMatrixModuleSimplify[metricMatrix0, order],
	gammaDownIndexMatrixModuleWithoutSimplify[metricMatrix0, order]]


(* Subscript[\[CapitalGamma]^a, bc], i.e., affine connection: *)

affineConnectionMatrixModule[metricMatrix0_, order_:0, simplifyQ_:True] := 
If[simplifyQ == True,
	affineConnectionMatrixModuleSimplify[metricMatrix0, order],
	affineConnectionMatrixModuleWithoutSimplify[metricMatrix0, order]]


(* Subscript[R^a, bcd]: *)

rUpIndexMatrixModule[metricMatrix0_, order_:0, simplifyQ_:True] := 
If[simplifyQ == True,
	rUpIndexMatrixModuleSimplify[metricMatrix0, order],
	rUpIndexMatrixModuleWithoutSimplify[metricMatrix0, order]]


(* Subscript[R, abcd], if you use it. (Or, you can make it to be just \
a comment, if you will not use it at all!): *)

rDownIndexMatrixModule[metricMatrix0_, order_:0, simplifyQ_:True] := 
If[simplifyQ == True,
	rDownIndexMatrixModuleSimplify[metricMatrix0, order],
	rDownIndexMatrixModuleWithoutSimplify[metricMatrix0, order]]


(* Subscript[R, ab], i.e., Ricci tensor: *)

ricciTensorMatrixModule[metricMatrix0_, order_:0, simplifyQ_:True] := 
If[simplifyQ == True,
	ricciTensorMatrixModuleSimplify[metricMatrix0, order],
	ricciTensorMatrixModuleWithoutSimplify[metricMatrix0, order]]


(* R, i.e., Ricci scalar: *)

ricciScalarMatrixModule[metricMatrix0_, order_:0, simplifyQ_:True] :=
If[simplifyQ == True,
	ricciScalarMatrixModuleSimplify[metricMatrix0, order],
	ricciScalarMatrixModuleWithoutSimplify[metricMatrix0, order]]


(* Subscript[G, ab], i.e., Einstein tensor: *)

einsteinTensorMatrixModule[metricMatrix0_, order_:0, simplifyQ_:True] :=
If[simplifyQ == True,
	einsteinTensorMatrixModuleSimplify[metricMatrix0, order],
	einsteinTensorMatrixModuleWithoutSimplify[metricMatrix0, order]]


(* (G^ab): *)

einsteinTensorUpIndexMatrixModule[metricMatrix0_, order_:0, simplifyQ_:True] :=
If[simplifyQ == True,
	einsteinTensorUpIndexMatrixModuleSimplify[metricMatrix0, order],
	einsteinTensorUpIndexMatrixModuleWithoutSimplify[metricMatrix0, order]]
