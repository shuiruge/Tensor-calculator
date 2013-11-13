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

metricUpIndexMatrixModule[metricMatrix0_] := 
  Module[{metricMatrix = metricMatrix0, 
    metricUpIndexMatrix0  = Table[0, {i0, dim}, {i2, dim}]},
   
   metricUpIndexMatrix0 = Simplify[Normal[Series[Inverse[metricMatrix], {\[Epsilon]1, 0, order}]]];
   metricUpIndexMatrix = metricUpIndexMatrix0;
   metricUpIndexMatrix0
   ];


(* Subscript[\[CapitalGamma], abc]: *)

gammaDownIndexMatrixModule[metricMatrix0_] := 
 Module[{metricMatrix = metricMatrix0, 
   gammaDownIndexMatrix0 = Table[0, {i0, dim}, {i2, dim}, {i3, dim}]},
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		For[k0 = 1, k0 <= dim, k0++,
      			gammaDownIndexMatrix0[[i0, j0, k0]] = 
       Simplify[Normal[Series[
        1/2*( D[metricMatrix[[i0, k0]], coordinates[[j0]]] + D[metricMatrix[[i0, j0]], coordinates[[k0]]] - D[metricMatrix[[j0, k0]], coordinates[[i0]]] )
				, {\[Epsilon]1, 0, order}]]]
      		]
     	]
    ]
   ];
  gammaDownIndexMatrix = gammaDownIndexMatrix0;
  gammaDownIndexMatrix0
  ]


(* Subscript[\[CapitalGamma]^a, bc], i.e., affine connection: *)

affineConnectionMatrixModule[metricMatrix0_] := 
 Module[{metricMatrix = metricMatrix0, 
   affineConnectionMatrix0 = Table[0, {i0, dim}, {i2, dim}, {i3, dim}]},
  
  metricUpIndexMatrixModule[metricMatrix]; (* 
  Write it down here, hence we get the variable "metricUpIndex", for preventing that the metricUpIndexModule[
  metric] "read" every time when preforming the 'For' circle! This is \
just a skill! Note that, 
  the 'gMetricUpIndex' is no longer a local variable! *)
  
  gammaDownIndexMatrixModule[metricMatrix];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		For[k0 = 1, k0 <= dim, k0++,
      			affineConnectionMatrix0[[i0, j0, k0]] = 
       Simplify[Normal[Series[
        Sum[metricUpIndexMatrix[[i0, m0]]*gammaDownIndexMatrix[[m0, j0, k0]], {m0, 1, dim}]
				, {\[Epsilon]1, 0, order}]]]
      		]
     	]
    ]
   ];
  affineConnectionMatrix = affineConnectionMatrix0;
  affineConnectionMatrix0
  ]


(* Subscript[R^a, bcd]: *)

rUpIndexMatrixModule[metricMatrix0_] := 
 Module[{metricMatrix = metricMatrix0, 
   rUpIndexMatrix0 = 
    Table[0, {i0, dim}, {i2, dim}, {i3, dim}, {i4, dim}]},
  
  affineConnectionMatrixModule[metricMatrix];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		For[k0 = 1, k0 <= dim, k0++,
      			For[l0 = 1, l0 <= dim, l0++,
       			
       rUpIndexMatrix0[[i0, j0, k0, l0]] = 
        Simplify[Normal[Series[
         D[affineConnectionMatrix[[i0, j0, l0]], coordinates[[k0]]] - 
          D[affineConnectionMatrix[[i0, j0, k0]], coordinates[[l0]]] + 
          Sum[(affineConnectionMatrix[[i0, k0, m0]])*(affineConnectionMatrix[[m0, j0, 
              l0]]), {m0, 1, dim}] - 
          Sum[(affineConnectionMatrix[[i0, l0, m0]])*(affineConnectionMatrix[[m0, j0, 
              k0]]), {m0, 1, dim}]
       			, {\[Epsilon]1, 0, order}]]]
				]
      		]
     	]
    ]
   ];
  rUpIndexMatrix = rUpIndexMatrix0;
  rUpIndex0Matrix
  ]


(* Subscript[R, abcd], if you use it. (Or, you can make it to be just \
a comment, if you will not use it at all!): *)

rDownIndexMatrixModule[metricMatrix0_] := 
 Module[{metricMatrix = metricMatrix0, 
   rDownIndexMatrix0 = 
    Table[0, {i0, dim}, {i2, dim}, {i3, dim}, {i4, dim}]},
  
  rUpIndexMatrixModule[metricMatrix];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		For[k0 = 1, k0 <= dim, k0++,
      			For[l0 = 1, l0 <= dim, l0++,
       				
       rDownIndexMatrix0[[i0, j0, k0, l0]] = 
        Simplify[Normal[Series[
         Sum[(metricMatrix[[i0, m0]])*(rUpIndexMatrix[[m0, j0, k0, l0]]), {m0, 1, 
           dim}]
		, {\[Epsilon]1, 0, order}]]]
       			]
      		]
     	]
    ]
   ];
  rDownIndexMatrix = rDownIndexMatrix0;
  rDownIndexMatrix0
  ]


(* Subscript[R, ab], i.e., Ricci tensor: *)

ricciTensorMatrixModule[metricMatrix0_] := 
 Module[{metricMatrix = metricMatrix0, 
   ricciTensorMatrix0 = Table[0, {i0, dim}, {i2, dim}]},
  
  rUpIndexMatrixModule[metricMatrix];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		ricciTensorMatrix0[[i0, j0]] = 
      Simplify[Normal[Series[Sum[rUpIndexMatrix[[m0, i0, m0, j0]], {m0, 1, dim}], {\[Epsilon]1, 0, order}]]]
     		(* Or, you can write:
     					For[k0=1,k0<=dim,k0++,
     						ricciTensor[[i0,j0]]=ricciTensor[[i0,j0]]+rUpIndex[[k0,i0,k0,j0]]
     					]
     		*)
     	]
    ]
   ];
  ricciTensorMatrix = ricciTensorMatrix0;
  ricciTensorMatrix0
  ]


(* R, i.e., Ricci scalar: *)

ricciScalarMatrixModule[metricMatrix0_] := 
 Module[{metricMatrix = metricMatrix0, ricciScalarMatrix0 = 0},
  
  ricciTensorMatrixModule[metricMatrix];
  
  ricciScalarMatrix0 = 
   Simplify[Normal[Series[
    Sum[(metricUpIndexMatrix[[i0, j0]])*(ricciTensorMatrix[[i0,j0]]), {i0, 1, 
      dim}, {j0, 1, dim}], {\[Epsilon]1, 0, order}]]];
  ricciScalarMatrix = ricciScalarMatrix0;
  ricciScalarMatrix0
  ]


(* Subscript[G, ab], i.e., Einstein tensor: *)

einsteinTensorMatrixModule[metricMatrix0_] := 
 Module[{metricMatrix = metricMatrix0, 
   einsteinTensorMatrix0 = Table[0, {i0, dim}, {i2, dim}]},
  
  ricciScalarMatrixModule[metricMatrix];
  
  Module[{i0, j0, k0, l0},
   For[i0 = 1, i0 <= dim, i0++,
    	For[j0 = 1, j0 <= dim, j0++,
     		einsteinTensorMatrix0[[i0, j0]] = 
      Simplify[Normal[Series[
       ricciTensorMatrix[[i0, j0]] - (1/2)*(ricciScalarMatrix)*(metricMatrix[[i0, j0]])
		, {\[Epsilon]1, 0, order}]]]
     	]
    ]
   ];
  einsteinTensorMatrix = einsteinTensorMatrix0;
  einsteinTensorMatrix0
  ]


(* (G^ab): *)

einsteinTensorUpIndexMatrixModule[metricMatrix0_] := 
 Module[{metricMatrix = metricMatrix0, 
   einsteinTensorUpIndexMatrix0 = Table[0, {i0, dim}, {i2, dim}]},
  
  einsteinTensorMatrixModule[metricMatrix];
  
  Module[{i0, j0, k0, l0},
    For[i0 = 1, i0 <= dim, i0++,
     For[j0 = 1, j0 <= dim, j0++,
      einsteinTensorUpIndexMatrix0[[i0, j0]] = 
        Simplify[Normal[Series[
			Sum[metricUpIndexMatrix[[i0, k0]]*metricUpIndexMatrix[[j0, l0]]*
          einsteinTensorMatrix[[k0, l0]], {k0, 1, dim}, {l0, 1, dim}]
		, {\[Epsilon]1, 0, order}]]];
      ]
     ]
    ];
   einsteinTensorUpIndexMatrix = einsteinTensorUpIndexMatrix0;
   einsteinTensorUpIndexMatrix0
  ]
