(* ::Package:: *)

helpMeTensorCalculator = Print["'metricUpIndexModule[metric]' for\!\(\*FormBox[SuperscriptBox[\(g\), \(ab\)],
TraditionalForm]\),where g is the metric;\n'gammaDownIndexModule[metric]' for\!\(\*FormBox[SubscriptBox[\(\[CapitalGamma]\), \(abc\)],
TraditionalForm]\);\n'affineConnectionModule[metric]' for\!\(\*FormBox[SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(a\)], \(bc\)],
TraditionalForm]\),i.e.,the affine connection;\n'rUpIndexModule[metric]' for\!\(\*FormBox[SubscriptBox[SuperscriptBox[\(R\), \(a\)], \(bcd\)],
TraditionalForm]\);\n'rDownIndexModule[metric]' for\!\(\*FormBox[SubscriptBox[\(R\), \(abcd\)],
TraditionalForm]\);\n'ricciTensorModule[metric]' for\!\(\*FormBox[SubscriptBox[\(R\), \(ab\)],
TraditionalForm]\),i.e.,the Ricci tensor;\n'ricciScalarModule[metric]' for\!\(\*FormBox[\(R\),
TraditionalForm]\),i.e.,the Ricci scalar;\n'einsteinTensorModule[metric]' for\!\(\*FormBox[SubscriptBox[\(G\), \(ab\)],
TraditionalForm]\),i.e.,the Einstein tensor;\n'einsteinTensorUpIndexModule[metric]' for\!\(\*FormBox[SuperscriptBox[\(G\), \(ab\)],
TraditionalForm]\)."];


(* (g^ab): *)

metricUpIndexModule[metric0_] := 
  Module[{metric = metric0, 
    metricUpIndex0  = Table[0, {i1, nDim}, {i2, nDim}]},
   
   metricUpIndex0 = Simplify[Inverse[metric]];
   
   metricUpIndex0
   ];


(* Subscript[\[CapitalGamma], abc]: *)

gammaDownIndexModule[metric0_] := 
 Module[{metric = metric0, 
   gammaDownIndex0 = Table[0, {i1, nDim}, {i2, nDim}, {i3, nDim}]},
  
  Module[{i, j, k, l},
   For[i = 1, i <= nDim, i++,
    	For[j = 1, j <= nDim, j++,
     		For[k = 1, k <= nDim, k++,
      			gammaDownIndex0[[i, j, k]] = 
       Simplify[
        1/2*( D[metric[[i, k]], coordinates[[j]]] + D[metric[[i, j]], coordinates[[k]]] - 
           D[metric[[j, k]], coordinates[[i]]] ) ]
      		]
     	]
    ]
   ];
  
  gammaDownIndex0
  ]


(* Subscript[\[CapitalGamma]^a, bc], i.e., affine connection: *)

affineConnectionModule[metric0_] := 
 Module[{metric = metric0, 
   affineConnection0 = Table[0, {i1, nDim}, {i2, nDim}, {i3, nDim}]},
  
  metricUpIndex = metricUpIndexModule[metric]; (* 
  Write it down here, for preventing that the metricUpIndexModule[
  metric] "read" every time when preforming the 'For' circle! This is \
just a skill! Note that, 
  the 'gMetricUpIndex' is no longer a local variable! *)
  
  gammaDownIndex = gammaDownIndexModule[metric];
  
  Module[{i, j, k, l},
   For[i = 1, i <= nDim, i++,
    	For[j = 1, j <= nDim, j++,
     		For[k = 1, k <= nDim, k++,
      			affineConnection0[[i, j, k]] = 
       Simplify[
        Sum[metricUpIndex[[i, m]]*gammaDownIndex[[m, j, k]], {m, 1, 
          nDim}]]
      		]
     	]
    ]
   ];
  
  affineConnection0
  ]


(* Subscript[R^a, bcd]: *)

rUpIndexModule[metric0_] := 
 Module[{metric = metric0, 
   rUpIndex0 = 
    Table[0, {i1, nDim}, {i2, nDim}, {i3, nDim}, {i4, nDim}]},
  
  affineConnection = affineConnectionModule[metric];
  
  Module[{i, j, k, l},
   For[i = 1, i <= nDim, i++,
    	For[j = 1, j <= nDim, j++,
     		For[k = 1, k <= nDim, k++,
      			For[l = 1, l <= nDim, l++,
       			
       rUpIndex0[[i, j, k, l]] = 
        Simplify[
         D[affineConnection[[i, j, l]], coordinates[[k]]] - 
          D[affineConnection[[i, j, k]], coordinates[[l]]] + 
          Sum[(affineConnection[[i, k, m]])*(affineConnection[[m, j, 
              l]]), {m, 1, nDim}] - 
          Sum[(affineConnection[[i, l, m]])*(affineConnection[[m, j, 
              k]]), {m, 1, nDim}]]
       			]
      		]
     	]
    ]
   ];
  
  rUpIndex0
  ]


(* Subscript[R, abcd], if you use it. (Or, you can make it to be just \
a comment, if you will not use it at all!): *)

rDownIndexModule[metric0_] := 
 Module[{metric = metric0, 
   rDownIndex0 = 
    Table[0, {i1, nDim}, {i2, nDim}, {i3, nDim}, {i4, nDim}]},
  
  rUpIndex = rUpIndexModule[metric];
  
  Module[{i, j, k, l},
   For[i = 1, i <= nDim, i++,
    	For[j = 1, j <= nDim, j++,
     		For[k = 1, k <= nDim, k++,
      			For[l = 1, l <= nDim, l++,
       				
       rDownIndex0[[i, j, k, l]] = 
        Simplify[
         Sum[(metric[[i, m]])*(rUpIndex[[m, j, k, l]]), {m, 1, 
           nDim}]]
       			]
      		]
     	]
    ]
   ];
  
  rDownIndex0
  ]


(* Subscript[R, ab], i.e., Ricci tensor: *)

ricciTensorModule[metric0_] := 
 Module[{metric = metric0, 
   ricciTensor0 = Table[0, {i1, nDim}, {i2, nDim}]},
  
  rUpIndex = rUpIndexModule[metric];
  
  Module[{i, j, k, l},
   For[i = 1, i <= nDim, i++,
    	For[j = 1, j <= nDim, j++,
     		ricciTensor0[[i, j]] = 
      Simplify[Sum[rUpIndex[[m, i, m, j]], {m, 1, nDim}]]
     		(* Or, you can write:
     					For[k=1,k<=nDim,k++,
     						ricciTensor[[i,j]]=ricciTensor[[i,j]]+rUpIndex[[k,i,k,j]]
     					]
     		*)
     	]
    ]
   ];
  
  ricciTensor0
  ]


(* R, i.e., Ricci scalar: *)

ricciScalarModule[metric0_] := 
 Module[{metric = metric0, ricciScalar0 = 0},
  
  ricciTensor = ricciTensorModule[metric];
  
  ricciScalar0 = 
   Simplify[
    Sum[(metricUpIndex[[m, nDim]])*(ricciTensor[[m, nDim]]), {m, 1, 
      nDim}, {nDim, 1, nDim}]];
  
  ricciScalar0
  ]


(* Subscript[G, ab], i.e., Einstein tensor: *)

einsteinTensorModule[metric0_] := 
 Module[{metric = metric0, 
   einsteinTensor0 = Table[0, {i1, nDim}, {i2, nDim}]},
  
  ricciScalar = ricciScalarModule[metric];
  
  Module[{i, j, k, l},
   For[i = 1, i <= nDim, i++,
    	For[j = 1, j <= nDim, j++,
     		einsteinTensor0[[i, j]] = 
      Simplify[
       ricciTensor[[i, j]] - (1/2)*(ricciScalar)*(metric[[i, j]])]
     	]
    ]
   ];
  
  einsteinTensor0
  ]


(* (G^ab): *)

einsteinTensorUpIndexModule[metric0_] := 
 Module[{metric = metric0, 
   einsteinTensorUpIndex0 = Table[0, {i1, nDim}, {i2, nDim}]},
  
  einsteinTensor = einsteinTensorModule[metric];
  
  Module[{i, j, k, l},
    For[i = 1, i <= nDim, i++,
     For[j = 1, j <= nDim, j++,
      einsteinTensorUpIndex0[[i, j]] = 
        Sum[metricUpIndex[[i, k]]*metricUpIndex[[j, l]]*
          einsteinTensor[[k, l]], {k, 1, nDim}, {l, 1, nDim}];
      ]
     ]
    ];
   
   einsteinTensorUpIndex0
  ]
