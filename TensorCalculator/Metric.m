
(* Walker-Robertson metric with K=0 under Cartesian coordinate: *)
wrMetric = walkerRobertsonMetric = {{-a[x[4]]^2, 0, 0, 0}, {0, -a[x[4]]^2, 0, 0}, {0, 0, -a[x[4]]^2, 0}, {0, 0, 0, 1}};

(* Schwarzschild metric, under spherical coordinate, denoting r=x[1], \[Theta]=x[2], \[Phi]=x[3], t=x[4] and the Schwarzschild radius=r[S]: *)
schwarzschildMetric = {{-(1 - r[S]/x[1])^-1, 0, 0, 0}, {0, -x[1]^2, 0, 0}, {0, 0, -x[1]^2*Sin[x[2]]^2, 0}, {0, 0, 0, 1 - r[S]/x[1]}};

gpMetric = gullstrandPainleveMetric = {{-1, 0, 0, -Sqrt[(r[S]/x[1])]}, {0, -x[1]^2, 0, 0}, {0, 0, -x[1]^2*Sin[x[2]]^2, 0}, {-Sqrt[(r[S]/x[1])], 0, 0, - r[S]/x[1]}};

(* ********************************* *)

