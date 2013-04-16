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
    metricUpIndex0  = Table[0, {i0, dim}, {i2, dim}]},
   
   metricUpIndex0 = Simplify[Inverse[metric]];
   metricUpIndex = metricUpIndex0;
   metricUpIndex0
   ];


(* Subscript[\[CapitalGamma], abc]: *)

gammaDownIndexModule[metric0_] := 
 Module[{metric = metric0, 
   gammaDownIndex0 = Table[0, {i0, dim}, {i2, dim}, {i3, dim}]},
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		For[k0 = 1, k0 <= dim, k0++,
      			gammaDownIndex0[[i0, j0, k0]] = 
       Simplify[
        1/2*( D[metric[[i0, k0]], coordinates[[j0]]] + D[metric[[i0, j0]], coordinates[[k0]]] - D[metric[[j0, k0]], coordinates[[i0]]] )
				]
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
   affineConnection0 = Table[0, {i0, dim}, {i2, dim}, {i3, dim}]},
  
  metricUpIndexModule[metric]; (* 
  Write it down here, hence we get the variable "metricUpIndex", for preventing that the metricUpIndexModule[
  metric] "read" every time when preforming the 'For' circle! This is \
just a skill! Note that, 
  the 'gMetricUpIndex' is no longer a local variable! *)
  
  gammaDownIndexModule[metric];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		For[k0 = 1, k0 <= dim, k0++,
      			affineConnection0[[i0, j0, k0]] = 
       Simplify[
        Sum[metricUpIndex[[i0, m0]]*gammaDownIndex[[m0, j0, k0]], {m0, 1, dim}]
				]
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
    Table[0, {i0, dim}, {i2, dim}, {i3, dim}, {i4, dim}]},
  
  affineConnectionModule[metric];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		For[k0 = 1, k0 <= dim, k0++,
      			For[l0 = 1, l0 <= dim, l0++,
       			
       rUpIndex0[[i0, j0, k0, l0]] = 
        Simplify[
         D[affineConnection[[i0, j0, l0]], coordinates[[k0]]] - 
          D[affineConnection[[i0, j0, k0]], coordinates[[l0]]] + 
          Sum[(affineConnection[[i0, k0, m0]])*(affineConnection[[m0, j0, 
              l0]]), {m0, 1, dim}] - 
          Sum[(affineConnection[[i0, l0, m0]])*(affineConnection[[m0, j0, 
              k0]]), {m0, 1, dim}]]
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
    Table[0, {i0, dim}, {i2, dim}, {i3, dim}, {i4, dim}]},
  
  rUpIndexModule[metric];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		For[k0 = 1, k0 <= dim, k0++,
      			For[l0 = 1, l0 <= dim, l0++,
       				
       rDownIndex0[[i0, j0, k0, l0]] = 
        Simplify[
         Sum[(metric[[i0, m0]])*(rUpIndex[[m0, j0, k0, l0]]), {m0, 1, 
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
   ricciTensor0 = Table[0, {i0, dim}, {i2, dim}]},
  
  rUpIndexModule[metric];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		ricciTensor0[[i0, j0]] = 
      Simplify[Sum[rUpIndex[[m0, i0, m0, j0]], {m0, 1, dim}]]
     		(* Or, you can write:
     					For[k0=1,k0<=dim,k0++,
     						ricciTensor[[i0,j0]]=ricciTensor[[i0,j0]]+rUpIndex[[k0,i0,k0,j0]]
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
    Sum[(metricUpIndex[[i0, j0]])*(ricciTensor[[i0,j0]]), {i0, 1, 
      dim}, {j0, 1, dim}]];
  ricciScalar = ricciScalar0;
  ricciScalar0
  ]


(* Subscript[G, ab], i.e., Einstein tensor: *)

einsteinTensorModule[metric0_] := 
 Module[{metric = metric0, 
   einsteinTensor0 = Table[0, {i0, dim}, {i2, dim}]},
  
  ricciScalarModule[metric];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		einsteinTensor0[[i0, j0]] = 
      Simplify[
       ricciTensor[[i0, j0]] - (1/2)*(ricciScalar)*(metric[[i0, j0]])]
     	]
    ]
   ];
  einsteinTensor = einsteinTensor0;
  einsteinTensor0
  ]


(* (G^ab): *)

einsteinTensorUpIndexModule[metric0_] := 
 Module[{metric = metric0, 
   einsteinTensorUpIndex0 = Table[0, {i0, dim}, {i2, dim}]},
  
  einsteinTensorModule[metric];
  
  Module[{i0, j0, k0, l0},
    For[i0 = 1, i0 <= dim, i0++,
     For[j0 = 1, j0 <= dim, j0++,
      einsteinTensorUpIndex0[[i0, j0]] = 
        Sum[metricUpIndex[[i0, k0]]*metricUpIndex[[j0, l0]]*
          einsteinTensor[[k0, l0]], {k0, 1, dim}, {l0, 1, dim}];
      ]
     ]
    ];
   einsteinTensorUpIndex = einsteinTensorUpIndex0;
   einsteinTensorUpIndex0
  ]
