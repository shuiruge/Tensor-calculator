(* ::Package:: *)


(* (g^ab): *)

metricUpIndexMatrixModuleSimplify[metricMatrix0_, order_:0] := 
  Module[{metricMatrix = metricMatrix0},
   metricUpIndexMatrix = Simplify[Normal[Series[Inverse[metricMatrix], {\[Epsilon]1, 0, order}]]];
   metricUpIndexMatrix];


(* Subscript[\[CapitalGamma], abc]: *)

gammaDownIndexMatrixModuleSimplify[metricMatrix0_, order_:0] :=
Module[{metricMatrix = metricMatrix0},
       gammaDownIndexMatrix = Table[Simplify[Normal[Series[1/2*( D[metricMatrix[[i0, k0]], coordinates[[j0]]] + D[metricMatrix[[i0, j0]], coordinates[[k0]]] - D[metricMatrix[[j0, k0]], coordinates[[i0]]] ), {\[Epsilon]1, 0, order}]]], {i0, dim},{j0, dim},{k0, dim}];
       gammaDownIndexMatrix]


(* Subscript[\[CapitalGamma]^a, bc], i.e., affine connection: *)

affineConnectionMatrixModuleSimplify[metricMatrix0_, order_:0] := 
Module[{metricMatrix = metricMatrix0},
       metricUpIndexMatrixModuleSimplify[metricMatrix];
	(* Write it down here, hence we get the variable "metricUpIndex", for preventing that the metricUpIndexModule[metric] "read" every time when preforming the 'For' circle! This is just a skill! Note that, the 'gMetricUpIndex' is no longer a local variable! *)
       gammaDownIndexMatrixModuleSimplify[metricMatrix];
	affineConnectionMatrix = Table[Simplify[Normal[Series[Sum[metricUpIndexMatrix[[i0, m0]]*gammaDownIndexMatrix[[m0, j0, k0]], {m0, 1, dim}], {\[Epsilon]1, 0, order}]]], {i0, dim}, {j0, dim}, {k0, dim}];
       affineConnectionMatrix]


(* Subscript[R^a, bcd]: *)

rUpIndexMatrixModuleSimplify[metricMatrix0_, order_:0] := 
Module[{metricMatrix = metricMatrix0},
       affineConnectionMatrixModuleSimplify[metricMatrix];
       rUpIndexMatrix = Table[Simplify[Normal[Series[D[affineConnectionMatrix[[i0, j0, l0]], coordinates[[k0]]] - D[affineConnectionMatrix[[i0, j0, k0]], coordinates[[l0]]] + Sum[(affineConnectionMatrix[[i0, k0, m0]])*(affineConnectionMatrix[[m0, j0, l0]]), {m0, 1, dim}] - Sum[(affineConnectionMatrix[[i0, l0, m0]])*(affineConnectionMatrix[[m0, j0, k0]]), {m0, 1, dim}], {\[Epsilon]1, 0, order}]]], {i0, dim}, {j0, dim}, {k0, dim}, {l0, dim}];
       rUpIndexMatrix]


(* Subscript[R, abcd], if you use it. (Or, you can make it to be just \
a comment, if you will not use it at all!): *)

rDownIndexMatrixModuleSimplify[metricMatrix0_, order_:0] := 
Module[{metricMatrix = metricMatrix0},
       rUpIndexMatrixModuleSimplify[metricMatrix];
       rDownIndexMatrix = Table[Simplify[Normal[Series[Sum[(metricMatrix[[i0, m0]])*(rUpIndexMatrix[[m0, j0, k0, l0]]), {m0, 1, dim}], {\[Epsilon]1, 0, order}]]], {i0, dim}, {j0, dim}, {k0, dim}, {l0, dim}];
       rDownIndexMatrix]


(* Subscript[R, ab], i.e., Ricci tensor: *)

ricciTensorMatrixModuleSimplify[metricMatrix0_, order_:0] := 
Module[{metricMatrix = metricMatrix0},
       rUpIndexMatrixModuleSimplify[metricMatrix];
       ricciTensorMatrix = Table[Simplify[Normal[Series[Sum[rUpIndexMatrix[[m0, i0, m0, j0]], {m0, 1, dim}], {\[Epsilon]1, 0, order}]]], {i0, dim}, {j0, dim}];
       ricciTensorMatrix]


(* R, i.e., Ricci scalar: *)

ricciScalarMatrixModuleSimplify[metricMatrix0_, order_:0] :=
Module[{metricMatrix = metricMatrix0},
       ricciTensorMatrixModuleSimplify[metricMatrix];
       ricciScalarMatrix = Simplify[Normal[Series[Sum[(metricUpIndexMatrix[[i0, j0]])*(ricciTensorMatrix[[i0,j0]]), {i0, 1, dim}, {j0, 1, dim}], {\[Epsilon]1, 0, order}]]];
       ricciScalarMatrix]


(* Subscript[G, ab], i.e., Einstein tensor: *)

einsteinTensorMatrixModuleSimplify[metricMatrix0_, order_:0] :=
Module[{metricMatrix = metricMatrix0},
	ricciScalarMatrixModuleSimplify[metricMatrix];
	einsteinTensorMatrix = Table[Simplify[Normal[Series[ricciTensorMatrix[[i0, j0]] - (1/2)*(ricciScalarMatrix)*(metricMatrix[[i0, j0]]), {\[Epsilon]1, 0, order}]]], {i0, dim}, {j0, dim}];
	einsteinTensorMatrix]


(* (G^ab): *)

einsteinTensorUpIndexMatrixModuleSimplify[metricMatrix0_, order_:0] :=
Module[{metricMatrix = metricMatrix0},
       einsteinTensorMatrixModuleSimplify[metricMatrix];
       einsteinTensorUpIndexMatrix = Table[Simplify[Normal[Series[Sum[metricUpIndexMatrix[[i0, k0]]*metricUpIndexMatrix[[j0, l0]]*einsteinTensorMatrix[[k0, l0]], {k0, 1, dim}, {l0, 1, dim}], {\[Epsilon]1, 0, order}]]], {i0, dim}, {j0, dim}];
       einsteinTensorUpIndexMatrix]
