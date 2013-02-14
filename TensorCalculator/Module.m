(* ::Package:: *)

(* (g^ab): *)

gMetricUpIndexModule[metric0_] := 
  Module[{metric = metric0, 
    gMetricUpIndex0  = Table[0, {i1, nDim}, {i2, nDim}]},
   
   gMetricUpIndex0 = Simplify[Inverse[metric]];
   
   gMetricUpIndex0
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
        1/2*( D[metric[[i, k]], x[j]] + D[metric[[i, j]], x[k]] - 
           D[metric[[j, k]], x[i]] ) ]
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
  
  gMetricUpIndex = gMetricUpIndexModule[metric]; (* 
  Write it down here, for preventing that the gMetricUpIndexModule[
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
        Sum[gMetricUpIndex[[i, m]]*gammaDownIndex[[m, j, k]], {m, 1, 
          nDim}]]
      		]
     	]
    ]
   ];
  
  affineConnection0
  ]


(* Subscript[(R^a), bcd]: *)

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
         D[affineConnection[[i, j, l]], x[k]] - 
          D[affineConnection[[i, j, k]], x[l]] + 
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
    Sum[(gMetricUpIndex[[m, nDim]])*(ricciTensor[[m, nDim]]), {m, 1, 
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
        Sum[gMetricUpIndex[[i, k]]*gMetricUpIndex[[j, l]]*
          einsteinTensor[[k, l]], {k, 1, nDim}, {l, 1, nDim}];
      ]
     ]
    ];
   
   einsteinTensorUpIndex0
  ]
