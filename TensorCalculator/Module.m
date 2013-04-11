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
    metricUpIndex0  = Table[0, {i1, dim}, {i2, dim}]},
   
   metricUpIndex0 = Simplify[Inverse[metric]];
   metricUpIndex = metricUpIndex0;
   metricUpIndex0
   ];


(* Subscript[\[CapitalGamma], abc]: *)

gammaDownIndexModule[metric0_] := 
 Module[{metric = metric0, 
   gammaDownIndex0 = Table[0, {i1, dim}, {i2, dim}, {i3, dim}]},
  
  Module[{i1, j1, k1, l1},
   For[i1 = 1, i1 <= dim, i1++,
    	For[j1 = 1, j1 <= dim, j1++,
     		For[k1 = 1, k1 <= dim, k1++,
      			gammaDownIndex0[[i1, j1, k1]] = 
       Simplify[
        1/2*( D[metric[[i1, k1]], coordinates[[j1]]] + D[metric[[i1, j1]], coordinates[[k1]]] - 
           D[metric[[j1, k1]], coordinates[[i1]]] ) ]
      		]
     	]
    ]
   ];
  gammaDownIndex = gammaDownIndex0;
  gammaDownIndex0
  ]


(* Subscript[\[CapitalGamma]^a, bc], i.e., affine connection: *)

affineConnectionModule[metric0_] := 
 Module[{metric = metric0, 
   affineConnection0 = Table[0, {i1, dim}, {i2, dim}, {i3, dim}]},
  
  metricUpIndexModule[metric]; (* 
  Write it down here, hence we get the variable "metricUpIndex", for preventing that the metricUpIndexModule[
  metric] "read" every time when preforming the 'For' circle! This is \
just a skill! Note that, 
  the 'gMetricUpIndex' is no longer a local variable! *)
  
  gammaDownIndexModule[metric];
  
  Module[{i1, j1, k1, l1},
   For[i1 = 1, i1 <= dim, i1++,
    	For[j1 = 1, j1 <= dim, j1++,
     		For[k1 = 1, k1 <= dim, k1++,
      			affineConnection0[[i1, j1, k1]] = 
       Simplify[
        Sum[metricUpIndex[[i1, m1]]*gammaDownIndex[[m1, j1, k1]], {m1, 1, 
          dim}]]
      		]
     	]
    ]
   ];
  affineConnection = affineConnection0;
  affineConnection0
  ]


(* Subscript[R^a, bcd]: *)

rUpIndexModule[metric0_] := 
 Module[{metric = metric0, 
   rUpIndex0 = 
    Table[0, {i1, dim}, {i2, dim}, {i3, dim}, {i4, dim}]},
  
  affineConnectionModule[metric];
  
  Module[{i1, j1, k1, l1},
   For[i1 = 1, i1 <= dim, i1++,
    	For[j1 = 1, j1 <= dim, j1++,
     		For[k1 = 1, k1 <= dim, k1++,
      			For[l1 = 1, l1 <= dim, l1++,
       			
       rUpIndex0[[i1, j1, k1, l1]] = 
        Simplify[
         D[affineConnection[[i1, j1, l1]], coordinates[[k1]]] - 
          D[affineConnection[[i1, j1, k1]], coordinates[[l1]]] + 
          Sum[(affineConnection[[i1, k1, m1]])*(affineConnection[[m1, j1, 
              l1]]), {m1, 1, dim}] - 
          Sum[(affineConnection[[i1, l1, m1]])*(affineConnection[[m1, j1, 
              k1]]), {m1, 1, dim}]]
       			]
      		]
     	]
    ]
   ];
  rUpIndex = rUpIndex0;
  rUpIndex0
  ]


(* Subscript[R, abcd], if you use it. (Or, you can make it to be just \
a comment, if you will not use it at all!): *)

rDownIndexModule[metric0_] := 
 Module[{metric = metric0, 
   rDownIndex0 = 
    Table[0, {i1, dim}, {i2, dim}, {i3, dim}, {i4, dim}]},
  
  rUpIndexModule[metric];
  
  Module[{i1, j1, k1, l1},
   For[i1 = 1, i1 <= dim, i1++,
    	For[j1 = 1, j1 <= dim, j1++,
     		For[k1 = 1, k1 <= dim, k1++,
      			For[l1 = 1, l1 <= dim, l1++,
       				
       rDownIndex0[[i1, j1, k1, l1]] = 
        Simplify[
         Sum[(metric[[i1, m1]])*(rUpIndex[[m1, j1, k1, l1]]), {m1, 1, 
           dim}]]
       			]
      		]
     	]
    ]
   ];
  rDownIndex = rDownIndex0;
  rDownIndex0
  ]


(* Subscript[R, ab], i.e., Ricci tensor: *)

ricciTensorModule[metric0_] := 
 Module[{metric = metric0, 
   ricciTensor0 = Table[0, {i1, dim}, {i2, dim}]},
  
  rUpIndexModule[metric];
  
  Module[{i1, j1, k1, l1},
   For[i1 = 1, i1 <= dim, i1++,
    	For[j1 = 1, j1 <= dim, j1++,
     		ricciTensor0[[i1, j1]] = 
      Simplify[Sum[rUpIndex[[m1, i1, m1, j1]], {m1, 1, dim}]]
     		(* Or, you can write:
     					For[k1=1,k1<=dim,k1++,
     						ricciTensor[[i1,j1]]=ricciTensor[[i1,j1]]+rUpIndex[[k1,i1,k1,j1]]
     					]
     		*)
     	]
    ]
   ];
  ricciTensor = ricciTensor0;
  ricciTensor0
  ]


(* R, i.e., Ricci scalar: *)

ricciScalarModule[metric0_] := 
 Module[{metric = metric0, ricciScalar0 = 0},
  
  ricciTensorModule[metric];
  
  ricciScalar0 = 
   Simplify[
    Sum[(metricUpIndex[[m1, dim]])*(ricciTensor[[m1, dim]]), {m1, 1, 
      dim}, {dim, 1, dim}]];
  ricciScalar = ricciScalar0;
  ricciScalar0
  ]


(* Subscript[G, ab], i.e., Einstein tensor: *)

einsteinTensorModule[metric0_] := 
 Module[{metric = metric0, 
   einsteinTensor0 = Table[0, {i1, dim}, {i2, dim}]},
  
  ricciScalarModule[metric];
  
  Module[{i1, j1, k1, l1},
   For[i1 = 1, i1 <= dim, i1++,
    	For[j1 = 1, j1 <= dim, j1++,
     		einsteinTensor0[[i1, j1]] = 
      Simplify[
       ricciTensor[[i1, j1]] - (1/2)*(ricciScalar)*(metric[[i1, j1]])]
     	]
    ]
   ];
  einsteinTensor = einsteinTensor0;
  einsteinTensor0
  ]


(* (G^ab): *)

einsteinTensorUpIndexModule[metric0_] := 
 Module[{metric = metric0, 
   einsteinTensorUpIndex0 = Table[0, {i1, dim}, {i2, dim}]},
  
  einsteinTensorModule[metric];
  
  Module[{i1, j1, k1, l1},
    For[i1 = 1, i1 <= dim, i1++,
     For[j1 = 1, j1 <= dim, j1++,
      einsteinTensorUpIndex0[[i1, j1]] = 
        Sum[metricUpIndex[[i1, k1]]*metricUpIndex[[j1, l1]]*
          einsteinTensor[[k1, l1]], {k1, 1, dim}, {l1, 1, dim}];
      ]
     ]
    ];
   einsteinTensorUpIndex = einsteinTensorUpIndex0;
   einsteinTensorUpIndex0
  ]
