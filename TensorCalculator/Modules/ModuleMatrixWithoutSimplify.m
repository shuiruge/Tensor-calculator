(* ::Package:: *)


(* (g^ab): *)

metricUpIndexMatrixModuleWithoutSimplify[metricMatrix0_, order_:0] := 
  Module[{metricMatrix = metricMatrix0},
   metricUpIndexMatrix = Normal[Series[Inverse[metricMatrix], {\[Epsilon]1, 0, order}]];
   metricUpIndexMatrix];


(* Subscript[\[CapitalGamma], abc]: *)

gammaDownIndexMatrixModuleWithoutSimplify[metricMatrix0_, order_:0] :=
Module[{metricMatrix = metricMatrix0},
       gammaDownIndexMatrix = Table[Normal[Series[1/2*( D[metricMatrix[[i0, k0]], coordinates[[j0]]] + D[metricMatrix[[i0, j0]], coordinates[[k0]]] - D[metricMatrix[[j0, k0]], coordinates[[i0]]] ), {\[Epsilon]1, 0, order}]], {i0, dim},{j0, dim},{k0, dim}];
       gammaDownIndexMatrix]


(* Subscript[\[CapitalGamma]^a, bc], i.e., affine connection: *)

affineConnectionMatrixModuleWithoutSimplify[metricMatrix0_, order_:0] := 
Module[{metricMatrix = metricMatrix0},
       metricUpIndexMatrixModuleWithoutSimplify[metricMatrix];
	(* Write it down here, hence we get the variable "metricUpIndex", for preventing that the metricUpIndexModule[metric] "read" every time when preforming the 'For' circle! This is just a skill! Note that, the 'gMetricUpIndex' is no longer a local variable! *)
       gammaDownIndexMatrixModuleWithoutSimplify[metricMatrix];
	affineConnectionMatrix = Table[Normal[Series[Sum[metricUpIndexMatrix[[i0, m0]]*gammaDownIndexMatrix[[m0, j0, k0]], {m0, 1, dim}], {\[Epsilon]1, 0, order}]], {i0, dim}, {j0, dim}, {k0, dim}];
       affineConnectionMatrix]


(* Subscript[R^a, bcd]: *)

rUpIndexMatrixModuleWithoutSimplify[metricMatrix0_, order_:0] := 
Module[{metricMatrix = metricMatrix0},
       affineConnectionMatrixModuleWithoutSimplify[metricMatrix];
       rUpIndexMatrix = Table[Normal[Series[D[affineConnectionMatrix[[i0, j0, l0]], coordinates[[k0]]] - D[affineConnectionMatrix[[i0, j0, k0]], coordinates[[l0]]] + Sum[(affineConnectionMatrix[[i0, k0, m0]])*(affineConnectionMatrix[[m0, j0, l0]]), {m0, 1, dim}] - Sum[(affineConnectionMatrix[[i0, l0, m0]])*(affineConnectionMatrix[[m0, j0, k0]]), {m0, 1, dim}], {\[Epsilon]1, 0, order}]], {i0, dim}, {j0, dim}, {k0, dim}, {l0, dim}];
       rUpIndexMatrix]


(* Subscript[R, abcd], if you use it. (Or, you can make it to be just \
a comment, if you will not use it at all!): *)

rDownIndexMatrixModuleWithoutSimplify[metricMatrix0_, order_:0] := 
Module[{metricMatrix = metricMatrix0},
       rUpIndexMatrixModuleWithoutSimplify[metricMatrix];
       rDownIndexMatrix = Table[Normal[Series[Sum[(metricMatrix[[i0, m0]])*(rUpIndexMatrix[[m0, j0, k0, l0]]), {m0, 1, dim}], {\[Epsilon]1, 0, order}]], {i0, dim}, {j0, dim}, {k0, dim}, {l0, dim}];
       rDownIndexMatrix]


(* Subscript[R, ab], i.e., Ricci tensor: *)

ricciTensorMatrixModuleWithoutSimplify[metricMatrix0_, order_:0] := 
Module[{metricMatrix = metricMatrix0},
       rUpIndexMatrixModuleWithoutSimplify[metricMatrix];
       ricciTensorMatrix = Table[Normal[Series[Sum[rUpIndexMatrix[[m0, i0, m0, j0]], {m0, 1, dim}], {\[Epsilon]1, 0, order}]], {i0, dim}, {j0, dim}];
       ricciTensorMatrix]


(* R, i.e., Ricci scalar: *)

ricciScalarMatrixModuleWithoutSimplify[metricMatrix0_, order_:0] :=
Module[{metricMatrix = metricMatrix0},
       ricciTensorMatrixModuleWithoutSimplify[metricMatrix];
       ricciScalarMatrix = Normal[Series[Sum[(metricUpIndexMatrix[[i0, j0]])*(ricciTensorMatrix[[i0,j0]]), {i0, 1, dim}, {j0, 1, dim}], {\[Epsilon]1, 0, order}]];
       ricciScalarMatrix]


(* Subscript[G, ab], i.e., Einstein tensor: *)

einsteinTensorMatrixModuleWithoutSimplify[metricMatrix0_, order_:0] :=
Module[{metricMatrix = metricMatrix0},
	ricciScalarMatrixModuleWithoutSimplify[metricMatrix];
	einsteinTensorMatrix = Table[Normal[Series[ricciTensorMatrix[[i0, j0]] - (1/2)*(ricciScalarMatrix)*(metricMatrix[[i0, j0]]), {\[Epsilon]1, 0, order}]], {i0, dim}, {j0, dim}];
	einsteinTensorMatrix]


(* (G^ab): *)

einsteinTensorUpIndexMatrixModuleWithoutSimplify[metricMatrix0_, order_:0] :=
Module[{metricMatrix = metricMatrix0},
       einsteinTensorMatrixModuleWithoutSimplify[metricMatrix];
       einsteinTensorUpIndexMatrix = Table[Normal[Series[Sum[metricUpIndexMatrix[[i0, k0]]*metricUpIndexMatrix[[j0, l0]]*einsteinTensorMatrix[[k0, l0]], {k0, 1, dim}, {l0, 1, dim}], {\[Epsilon]1, 0, order}]], {i0, dim}, {j0, dim}];
       einsteinTensorUpIndexMatrix]
