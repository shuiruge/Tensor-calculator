(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Mathematica 9.0' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       157,          7]
NotebookDataLength[     26638,        679]
NotebookOptionsPosition[     25973,        655]
NotebookOutlinePosition[     26339,        671]
CellTagsIndexPosition[     26296,        668]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 RowBox[{"(*", "\n", 
  RowBox[{
   RowBox[{"helpMeTensorCalculator", " ", "=", " ", 
    RowBox[{
    "Print", "[", 
     "\"\<'metricUpIndexMatrixModule[metricMatrix]' \
for\\!\\(\\*FormBox[SuperscriptBox[\\(g\\), \\(ab\\)],\n\
TraditionalForm]\\),where g is the \
metric;\\n'gammaDownIndexMatrixModule[metricMatrix]' \
for\\!\\(\\*FormBox[SubscriptBox[\\(\[CapitalGamma]\\), \\(abc\\)],\n\
TraditionalForm]\\);\\n'affineConnectionModule[metric]' \
for\\!\\(\\*FormBox[SubscriptBox[SuperscriptBox[\\(\[CapitalGamma]\\), \
\\(a\\)], \\(bc\\)],\nTraditionalForm]\\),i.e.,the affine \
connection;\\n'rUpIndexMatrixModule[metricMatrix]' \
for\\!\\(\\*FormBox[SubscriptBox[SuperscriptBox[\\(R\\), \\(a\\)], \
\\(bcd\\)],\nTraditionalForm]\\);\\n'rDownIndexModule[metric]' \
for\\!\\(\\*FormBox[SubscriptBox[\\(R\\), \\(abcd\\)],\n\
TraditionalForm]\\);\\n'ricciTensorMatrixModule[metricMatrix]' \
for\\!\\(\\*FormBox[SubscriptBox[\\(R\\), \\(ab\\)],\n\
TraditionalForm]\\),i.e.,the Ricci \
tensor;\\n'ricciScalarMatrixModule[metricMatrix]' \
for\\!\\(\\*FormBox[\\(R\\),\nTraditionalForm]\\),i.e.,the Ricci \
scalar;\\n'einsteinTensorMatrixModule[metricMatrix]' \
for\\!\\(\\*FormBox[SubscriptBox[\\(G\\), \\(ab\\)],\n\
TraditionalForm]\\),i.e.,the Einstein \
tensor;\\n'einsteinTensorUpIndexMatrixModule[metricMatrix]' \
for\\!\\(\\*FormBox[SuperscriptBox[\\(G\\), \\(ab\\)],\n\
TraditionalForm]\\).\>\"", "]"}]}], ";"}], "\n", "*)"}]], "Code"],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"Protect", " ", "the", " ", "\"\<\[Epsilon]1\>\""}], ",", " ", 
    "which", ",", " ", "SPECIALLY", ",", " ", 
    RowBox[{"denotes", " ", "the", " ", "order", " ", "of", " ", "the", " ", 
     RowBox[{"perturbation", "."}]}]}], " ", "*)"}], "\n", "\n", 
  RowBox[{
   RowBox[{"Protect", "[", "\[Epsilon]1", "]"}], ";"}]}]], "Code"],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"(", 
     RowBox[{"g", "^", "ab"}], ")"}], ":"}], " ", "*)"}], "\n", "\n", 
  RowBox[{
   RowBox[{
    RowBox[{"metricUpIndexMatrixModuleWithoutSimplify", "[", 
     RowBox[{"metricMatrix0_", ",", " ", 
      RowBox[{"order_:", "0"}]}], "]"}], " ", ":=", " ", "\n", "  ", 
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{
        RowBox[{"metricMatrix", " ", "=", " ", "metricMatrix0"}], ",", " ", 
        "metricUpIndexMatrix0"}], "}"}], ",", "\n", "   ", 
      RowBox[{
       RowBox[{"metricUpIndexMatrix0", " ", "=", " ", 
        RowBox[{"Normal", "[", 
         RowBox[{"Series", "[", 
          RowBox[{
           RowBox[{"Inverse", "[", "metricMatrix", "]"}], ",", " ", 
           RowBox[{"{", 
            RowBox[{"\[Epsilon]1", ",", " ", "0", ",", " ", "order"}], 
            "}"}]}], "]"}], "]"}]}], ";", "\n", "   ", 
       RowBox[{"metricUpIndexMatrix", " ", "=", " ", "metricUpIndexMatrix0"}],
        ";", "\n", "   ", "metricUpIndexMatrix0"}]}], "\n", "   ", "]"}]}], 
   ";"}]}]], "Code",
 CellChangeTimes->{{3.613345270289034*^9, 3.613345271583406*^9}, {
  3.6133459373324137`*^9, 3.613345965803029*^9}, {3.613347035570372*^9, 
  3.6133470515159616`*^9}}],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"Subscript", "[", 
     RowBox[{"\[CapitalGamma]", ",", " ", "abc"}], "]"}], ":"}], " ", "*)"}], 
  "\n", "\n", 
  RowBox[{
   RowBox[{"gammaDownIndexMatrixModuleWithoutSimplify", "[", 
    RowBox[{"metricMatrix0_", ",", " ", 
     RowBox[{"order_:", "0"}]}], "]"}], " ", ":=", "\n", 
   RowBox[{"Module", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"metricMatrix", " ", "=", " ", "metricMatrix0"}], ",", " ", 
       "gammaDownIndexMatrix0"}], "}"}], ",", "\n", "\t", 
     RowBox[{
      RowBox[{"Table", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"gammaDownIndexMatrix0", "[", 
          RowBox[{"[", 
           RowBox[{"i0", ",", " ", "j0", ",", " ", "k0"}], "]"}], "]"}], " ", 
         "=", " ", 
         RowBox[{"Normal", "[", 
          RowBox[{"Series", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"1", "/", "2"}], "*", 
             RowBox[{"(", " ", 
              RowBox[{
               RowBox[{"D", "[", 
                RowBox[{
                 RowBox[{"metricMatrix", "[", 
                  RowBox[{"[", 
                   RowBox[{"i0", ",", " ", "k0"}], "]"}], "]"}], ",", " ", 
                 RowBox[{"coordinates", "[", 
                  RowBox[{"[", "j0", "]"}], "]"}]}], "]"}], " ", "+", " ", 
               RowBox[{"D", "[", 
                RowBox[{
                 RowBox[{"metricMatrix", "[", 
                  RowBox[{"[", 
                   RowBox[{"i0", ",", " ", "j0"}], "]"}], "]"}], ",", " ", 
                 RowBox[{"coordinates", "[", 
                  RowBox[{"[", "k0", "]"}], "]"}]}], "]"}], " ", "-", " ", 
               RowBox[{"D", "[", 
                RowBox[{
                 RowBox[{"metricMatrix", "[", 
                  RowBox[{"[", 
                   RowBox[{"j0", ",", " ", "k0"}], "]"}], "]"}], ",", " ", 
                 RowBox[{"coordinates", "[", 
                  RowBox[{"[", "i0", "]"}], "]"}]}], "]"}]}], " ", ")"}]}], 
            ",", " ", 
            RowBox[{"{", 
             RowBox[{"\[Epsilon]1", ",", " ", "0", ",", " ", "order"}], 
             "}"}]}], "]"}], "]"}]}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"i0", ",", " ", "dim"}], "}"}], ",", 
        RowBox[{"{", 
         RowBox[{"j0", ",", " ", "dim"}], "}"}], ",", 
        RowBox[{"{", 
         RowBox[{"k0", ",", " ", "dim"}], "}"}]}], "]"}], ";", "\n", "\t", 
      RowBox[{
      "gammaDownIndexMatrix", " ", "=", " ", "gammaDownIndexMatrix0"}], ";", 
      "\n", "\t", "gammaDownIndexMatrix0"}]}], "]"}]}]}]], "Code",
 CellChangeTimes->{
  3.6133452820992937`*^9, {3.6133453616066847`*^9, 3.613345575617395*^9}, {
   3.613345619346004*^9, 3.613345623641181*^9}, {3.6133458095248137`*^9, 
   3.613345813531394*^9}, {3.613345875951439*^9, 3.613345878527815*^9}, {
   3.613345945966426*^9, 3.61334595470242*^9}, {3.61334705686872*^9, 
   3.613347068467738*^9}}],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"Subscript", "[", 
     RowBox[{
      RowBox[{"\[CapitalGamma]", "^", "a"}], ",", " ", "bc"}], "]"}], ",", 
    " ", 
    RowBox[{"i", ".", "e", "."}], ",", " ", 
    RowBox[{"affine", " ", 
     RowBox[{"connection", ":"}]}]}], " ", "*)"}], "\n", "\n", 
  RowBox[{
   RowBox[{"affineConnectionMatrixModuleWithoutSimplify", "[", 
    RowBox[{"metricMatrix0_", ",", " ", 
     RowBox[{"order_:", "0"}]}], "]"}], " ", ":=", " ", "\n", 
   RowBox[{"Module", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"metricMatrix", " ", "=", " ", "metricMatrix0"}], ",", " ", 
       "affineConnectionMatrix0"}], "}"}], ",", "\n", "\t", 
     RowBox[{
      RowBox[{
      "metricUpIndexMatrixModuleWithoutSimplify", "[", "metricMatrix", "]"}], 
      ";", "\n", "\t", 
      RowBox[{"(*", " ", 
       RowBox[{
        RowBox[{"Write", " ", "it", " ", "down", " ", "here"}], ",", " ", 
        RowBox[{
        "hence", " ", "we", " ", "get", " ", "the", " ", "variable", " ", 
         "\"\<metricUpIndex\>\""}], ",", " ", 
        RowBox[{"for", " ", "preventing", " ", "that", " ", "the", " ", 
         RowBox[{"metricUpIndexModule", "[", "metric", "]"}], " ", 
         "\"\<read\>\"", " ", "every", " ", "time", " ", "when", " ", 
         "preforming", " ", 
         RowBox[{"the", " ", "'"}], 
         RowBox[{"For", "'"}], " ", 
         RowBox[{"circle", "!"}], " ", "This", " ", "is", " ", "just", " ", 
         "a", " ", 
         RowBox[{"skill", "!"}], " ", "Note", " ", "that"}], ",", " ", 
        RowBox[{
         RowBox[{"the", " ", "'"}], 
         RowBox[{"gMetricUpIndex", "'"}], " ", "is", " ", "no", " ", "longer",
          " ", "a", " ", "local", " ", 
         RowBox[{"variable", "!"}]}]}], " ", "*)"}], "\n", "\t", 
      RowBox[{
      "gammaDownIndexMatrixModuleWithoutSimplify", "[", "metricMatrix", "]"}],
       ";", "\n", "\t", 
      RowBox[{"Table", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"affineConnectionMatrix0", "[", 
          RowBox[{"[", 
           RowBox[{"i0", ",", " ", "j0", ",", " ", "k0"}], "]"}], "]"}], " ", 
         "=", " ", 
         RowBox[{"Normal", "[", 
          RowBox[{"Series", "[", 
           RowBox[{
            RowBox[{"Sum", "[", 
             RowBox[{
              RowBox[{
               RowBox[{"metricUpIndexMatrix", "[", 
                RowBox[{"[", 
                 RowBox[{"i0", ",", " ", "m0"}], "]"}], "]"}], "*", 
               RowBox[{"gammaDownIndexMatrix", "[", 
                RowBox[{"[", 
                 RowBox[{"m0", ",", " ", "j0", ",", " ", "k0"}], "]"}], 
                "]"}]}], ",", " ", 
              RowBox[{"{", 
               RowBox[{"m0", ",", " ", "1", ",", " ", "dim"}], "}"}]}], "]"}],
             ",", " ", 
            RowBox[{"{", 
             RowBox[{"\[Epsilon]1", ",", " ", "0", ",", " ", "order"}], 
             "}"}]}], "]"}], "]"}]}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"i0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"j0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"k0", ",", " ", "dim"}], "}"}]}], "]"}], ";", "\n", "\t", 
      RowBox[{
      "affineConnectionMatrix", " ", "=", " ", "affineConnectionMatrix0"}], 
      ";", "\n", "\t", "affineConnectionMatrix0"}]}], "]"}]}]}]], "Code",
 CellChangeTimes->{
  3.613345289557948*^9, {3.6133456012376842`*^9, 3.613345892673417*^9}, 
   3.613345974878931*^9, {3.6133470870144176`*^9, 3.613347116311761*^9}}],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"Subscript", "[", 
     RowBox[{
      RowBox[{"R", "^", "a"}], ",", " ", "bcd"}], "]"}], ":"}], " ", "*)"}], 
  "\n", "\n", 
  RowBox[{
   RowBox[{"rUpIndexMatrixModuleWithoutSimplify", "[", 
    RowBox[{"metricMatrix0_", ",", " ", 
     RowBox[{"order_:", "0"}]}], "]"}], " ", ":=", " ", "\n", 
   RowBox[{"Module", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"metricMatrix", " ", "=", " ", "metricMatrix0"}], ",", " ", 
       "rUpIndexMatrix0"}], "}"}], ",", "\n", "\t", 
     RowBox[{
      RowBox[{
      "affineConnectionMatrixModuleWithoutSimplify", "[", "metricMatrix", 
       "]"}], ";", "\n", "\t", 
      RowBox[{"Table", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"rUpIndexMatrix0", "[", 
          RowBox[{"[", 
           RowBox[{"i0", ",", " ", "j0", ",", " ", "k0", ",", " ", "l0"}], 
           "]"}], "]"}], " ", "=", " ", 
         RowBox[{"Normal", "[", 
          RowBox[{"Series", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"D", "[", 
              RowBox[{
               RowBox[{"affineConnectionMatrix", "[", 
                RowBox[{"[", 
                 RowBox[{"i0", ",", " ", "j0", ",", " ", "l0"}], "]"}], "]"}],
                ",", " ", 
               RowBox[{"coordinates", "[", 
                RowBox[{"[", "k0", "]"}], "]"}]}], "]"}], " ", "-", " ", 
             RowBox[{"D", "[", 
              RowBox[{
               RowBox[{"affineConnectionMatrix", "[", 
                RowBox[{"[", 
                 RowBox[{"i0", ",", " ", "j0", ",", " ", "k0"}], "]"}], "]"}],
                ",", " ", 
               RowBox[{"coordinates", "[", 
                RowBox[{"[", "l0", "]"}], "]"}]}], "]"}], " ", "+", " ", 
             RowBox[{"Sum", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"(", 
                 RowBox[{"affineConnectionMatrix", "[", 
                  RowBox[{"[", 
                   RowBox[{"i0", ",", " ", "k0", ",", " ", "m0"}], "]"}], 
                  "]"}], ")"}], "*", 
                RowBox[{"(", 
                 RowBox[{"affineConnectionMatrix", "[", 
                  RowBox[{"[", 
                   RowBox[{"m0", ",", " ", "j0", ",", " ", "l0"}], "]"}], 
                  "]"}], ")"}]}], ",", " ", 
               RowBox[{"{", 
                RowBox[{"m0", ",", " ", "1", ",", " ", "dim"}], "}"}]}], 
              "]"}], " ", "-", " ", 
             RowBox[{"Sum", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"(", 
                 RowBox[{"affineConnectionMatrix", "[", 
                  RowBox[{"[", 
                   RowBox[{"i0", ",", " ", "l0", ",", " ", "m0"}], "]"}], 
                  "]"}], ")"}], "*", 
                RowBox[{"(", 
                 RowBox[{"affineConnectionMatrix", "[", 
                  RowBox[{"[", 
                   RowBox[{"m0", ",", " ", "j0", ",", " ", "k0"}], "]"}], 
                  "]"}], ")"}]}], ",", " ", 
               RowBox[{"{", 
                RowBox[{"m0", ",", " ", "1", ",", " ", "dim"}], "}"}]}], 
              "]"}]}], ",", " ", 
            RowBox[{"{", 
             RowBox[{"\[Epsilon]1", ",", " ", "0", ",", " ", "order"}], 
             "}"}]}], "]"}], "]"}]}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"i0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"j0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"k0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"l0", ",", " ", "dim"}], "}"}]}], "]"}], ";", "\n", "\t", 
      RowBox[{"rUpIndexMatrix", " ", "=", " ", "rUpIndexMatrix0"}], ";", "\n",
       "\t", "rUpIndexMatrix0"}]}], "]"}]}]}]], "Code",
 CellChangeTimes->{
  3.613345295076977*^9, 3.613345898816317*^9, {3.613345994441246*^9, 
   3.613346205156571*^9}, {3.613347122927161*^9, 3.613347138981453*^9}}],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"Subscript", "[", 
     RowBox[{"R", ",", " ", "abcd"}], "]"}], ",", " ", 
    RowBox[{
     RowBox[{"if", " ", "you", " ", "use", " ", 
      RowBox[{"it", ".", " ", 
       RowBox[{"(", 
        RowBox[{"Or", ",", " ", 
         RowBox[{
         "you", " ", "can", " ", "make", " ", "it", " ", "to", " ", "be", " ", 
          RowBox[{"just", " ", "\\\n", "a"}], " ", "comment"}], ",", " ", 
         RowBox[{
         "if", " ", "you", " ", "will", " ", "not", " ", "use", " ", "it", 
          " ", "at", " ", 
          RowBox[{"all", "!"}]}]}], ")"}]}]}], ":"}]}], " ", "*)"}], "\n", 
  "\n", 
  RowBox[{
   RowBox[{"rDownIndexMatrixModuleWithoutSimplify", "[", 
    RowBox[{"metricMatrix0_", ",", " ", 
     RowBox[{"order_:", "0"}]}], "]"}], " ", ":=", " ", "\n", 
   RowBox[{"Module", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"metricMatrix", " ", "=", " ", "metricMatrix0"}], ",", " ", 
       "rDownIndexMatrix0"}], "}"}], ",", "\n", "\t", 
     RowBox[{
      RowBox[{
      "rUpIndexMatrixModuleWithoutSimplify", "[", "metricMatrix", "]"}], ";", 
      "\n", "\t", 
      RowBox[{"Table", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"rDownIndexMatrix0", "[", 
          RowBox[{"[", 
           RowBox[{"i0", ",", " ", "j0", ",", " ", "k0", ",", " ", "l0"}], 
           "]"}], "]"}], " ", "=", " ", 
         RowBox[{"Normal", "[", 
          RowBox[{"Series", "[", 
           RowBox[{
            RowBox[{"Sum", "[", 
             RowBox[{
              RowBox[{
               RowBox[{"(", 
                RowBox[{"metricMatrix", "[", 
                 RowBox[{"[", 
                  RowBox[{"i0", ",", " ", "m0"}], "]"}], "]"}], ")"}], "*", 
               RowBox[{"(", 
                RowBox[{"rUpIndexMatrix", "[", 
                 RowBox[{"[", 
                  RowBox[{
                  "m0", ",", " ", "j0", ",", " ", "k0", ",", " ", "l0"}], 
                  "]"}], "]"}], ")"}]}], ",", " ", 
              RowBox[{"{", 
               RowBox[{"m0", ",", " ", "1", ",", " ", "dim"}], "}"}]}], "]"}],
             ",", " ", 
            RowBox[{"{", 
             RowBox[{"\[Epsilon]1", ",", " ", "0", ",", " ", "order"}], 
             "}"}]}], "]"}], "]"}]}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"i0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"j0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"k0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"l0", ",", " ", "dim"}], "}"}]}], "]"}], ";", "\n", "\t", 
      RowBox[{"rDownIndexMatrix", " ", "=", " ", "rDownIndexMatrix0"}], ";", 
      "\n", "\t", "rDownIndexMatrix0"}]}], "]"}]}]}]], "Code",
 CellChangeTimes->{
  3.6133453007899933`*^9, 3.613345902503046*^9, {3.613346245613316*^9, 
   3.613346372721396*^9}, {3.613347144280631*^9, 3.613347162211112*^9}}],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"Subscript", "[", 
     RowBox[{"R", ",", " ", "ab"}], "]"}], ",", " ", 
    RowBox[{"i", ".", "e", "."}], ",", " ", 
    RowBox[{"Ricci", " ", 
     RowBox[{"tensor", ":"}]}]}], " ", "*)"}], "\n", "\n", 
  RowBox[{
   RowBox[{"ricciTensorMatrixModuleWithoutSimplify", "[", 
    RowBox[{"metricMatrix0_", ",", " ", 
     RowBox[{"order_:", "0"}]}], "]"}], " ", ":=", " ", "\n", 
   RowBox[{"Module", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"metricMatrix", " ", "=", " ", "metricMatrix0"}], ",", " ", 
       "ricciTensorMatrix0"}], "}"}], ",", "\n", "\t", 
     RowBox[{
      RowBox[{
      "rUpIndexMatrixModuleWithoutSimplify", "[", "metricMatrix", "]"}], ";", 
      "\n", "    ", 
      RowBox[{"Table", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"ricciTensorMatrix0", "[", 
          RowBox[{"[", 
           RowBox[{"i0", ",", " ", "j0"}], "]"}], "]"}], " ", "=", " ", 
         RowBox[{"Normal", "[", 
          RowBox[{"Series", "[", 
           RowBox[{
            RowBox[{"Sum", "[", 
             RowBox[{
              RowBox[{"rUpIndexMatrix", "[", 
               RowBox[{"[", 
                RowBox[{
                "m0", ",", " ", "i0", ",", " ", "m0", ",", " ", "j0"}], "]"}],
                "]"}], ",", " ", 
              RowBox[{"{", 
               RowBox[{"m0", ",", " ", "1", ",", " ", "dim"}], "}"}]}], "]"}],
             ",", " ", 
            RowBox[{"{", 
             RowBox[{"\[Epsilon]1", ",", " ", "0", ",", " ", "order"}], 
             "}"}]}], "]"}], "]"}]}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"i0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"j0", ",", " ", "dim"}], "}"}]}], "]"}], ";", "\n", "\t", 
      RowBox[{"ricciTensorMatrix", " ", "=", " ", "ricciTensorMatrix0"}], ";",
       "\n", "\t", "ricciTensorMatrix0"}]}], "]"}]}]}]], "Code",
 CellChangeTimes->{
  3.613345307190014*^9, 3.613345905202396*^9, {3.613346383168861*^9, 
   3.61334645070757*^9}, {3.613347166686863*^9, 3.613347179755528*^9}}],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{"R", ",", " ", 
    RowBox[{"i", ".", "e", "."}], ",", " ", 
    RowBox[{"Ricci", " ", 
     RowBox[{"scalar", ":"}]}]}], " ", "*)"}], "\n", "\n", 
  RowBox[{
   RowBox[{"ricciScalarMatrixModuleWithoutSimplify", "[", 
    RowBox[{"metricMatrix0_", ",", " ", 
     RowBox[{"order_:", "0"}]}], "]"}], " ", ":=", "\n", 
   RowBox[{"Module", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"metricMatrix", " ", "=", " ", "metricMatrix0"}], ",", " ", 
       "ricciScalarMatrix0"}], "}"}], ",", "\n", "\t", 
     RowBox[{
      RowBox[{
      "ricciTensorMatrixModuleWithoutSimplify", "[", "metricMatrix", "]"}], 
      ";", "\n", "    ", 
      RowBox[{"ricciScalarMatrix0", " ", "=", " ", 
       RowBox[{"Normal", "[", 
        RowBox[{"Series", "[", 
         RowBox[{
          RowBox[{"Sum", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"(", 
              RowBox[{"metricUpIndexMatrix", "[", 
               RowBox[{"[", 
                RowBox[{"i0", ",", " ", "j0"}], "]"}], "]"}], ")"}], "*", 
             RowBox[{"(", 
              RowBox[{"ricciTensorMatrix", "[", 
               RowBox[{"[", 
                RowBox[{"i0", ",", "j0"}], "]"}], "]"}], ")"}]}], ",", " ", 
            RowBox[{"{", 
             RowBox[{"i0", ",", " ", "1", ",", " ", "dim"}], "}"}], ",", " ", 
            
            RowBox[{"{", 
             RowBox[{"j0", ",", " ", "1", ",", " ", "dim"}], "}"}]}], "]"}], 
          ",", " ", 
          RowBox[{"{", 
           RowBox[{"\[Epsilon]1", ",", " ", "0", ",", " ", "order"}], "}"}]}],
          "]"}], "]"}]}], ";", "\n", "\t", 
      RowBox[{"ricciScalarMatrix", " ", "=", " ", "ricciScalarMatrix0"}], ";",
       "\n", "\t", "ricciScalarMatrix0"}]}], "]"}]}]}]], "Code",
 CellChangeTimes->{
  3.613345312259035*^9, 3.6133459081905613`*^9, {3.613346459403236*^9, 
   3.613346498145722*^9}, {3.6133471912771187`*^9, 3.613347214148308*^9}}],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"Subscript", "[", 
     RowBox[{"G", ",", " ", "ab"}], "]"}], ",", " ", 
    RowBox[{"i", ".", "e", "."}], ",", " ", 
    RowBox[{"Einstein", " ", 
     RowBox[{"tensor", ":"}]}]}], " ", "*)"}], "\n", "\n", 
  RowBox[{
   RowBox[{"einsteinTensorMatrixModuleWithoutSimplify", "[", 
    RowBox[{"metricMatrix0_", ",", " ", 
     RowBox[{"order_:", "0"}]}], "]"}], " ", ":=", "\n", 
   RowBox[{"Module", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"metricMatrix", " ", "=", " ", "metricMatrix0"}], ",", " ", 
       "einsteinTensorMatrix0"}], "}"}], ",", "\n", "\t", 
     RowBox[{
      RowBox[{
      "ricciScalarMatrixModuleWithoutSimplify", "[", "metricMatrix", "]"}], 
      ";", "\n", "\t", 
      RowBox[{"Table", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"einsteinTensorMatrix0", "[", 
          RowBox[{"[", 
           RowBox[{"i0", ",", " ", "j0"}], "]"}], "]"}], " ", "=", " ", 
         RowBox[{"Normal", "[", 
          RowBox[{"Series", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"ricciTensorMatrix", "[", 
              RowBox[{"[", 
               RowBox[{"i0", ",", " ", "j0"}], "]"}], "]"}], " ", "-", " ", 
             RowBox[{
              RowBox[{"(", 
               RowBox[{"1", "/", "2"}], ")"}], "*", 
              RowBox[{"(", "ricciScalarMatrix", ")"}], "*", 
              RowBox[{"(", 
               RowBox[{"metricMatrix", "[", 
                RowBox[{"[", 
                 RowBox[{"i0", ",", " ", "j0"}], "]"}], "]"}], ")"}]}]}], ",",
             " ", 
            RowBox[{"{", 
             RowBox[{"\[Epsilon]1", ",", " ", "0", ",", " ", "order"}], 
             "}"}]}], "]"}], "]"}]}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"i0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"j0", ",", " ", "dim"}], "}"}]}], "]"}], ";", "\n", "\t", 
      RowBox[{
      "einsteinTensorMatrix", " ", "=", " ", "einsteinTensorMatrix0"}], ";", 
      "\n", "\t", "einsteinTensorMatrix0"}]}], "]"}]}]}]], "Code",
 CellChangeTimes->{
  3.6133453171239367`*^9, 3.613345911248159*^9, {3.6133465082298107`*^9, 
   3.61334662003907*^9}, {3.613347227388323*^9, 3.613347244194322*^9}}],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"(", 
     RowBox[{"G", "^", "ab"}], ")"}], ":"}], " ", "*)"}], "\n", "\n", 
  RowBox[{
   RowBox[{"einsteinTensorUpIndexMatrixModuleWithoutSimplify", "[", 
    RowBox[{"metricMatrix0_", ",", " ", 
     RowBox[{"order_:", "0"}]}], "]"}], " ", ":=", "\n", 
   RowBox[{"Module", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"metricMatrix", " ", "=", " ", "metricMatrix0"}], ",", " ", 
       "einsteinTensorUpIndexMatrix0"}], "}"}], ",", "\n", "\t", 
     RowBox[{
      RowBox[{
      "einsteinTensorMatrixModuleWithoutSimplify", "[", "metricMatrix", "]"}],
       ";", "\n", "\t", 
      RowBox[{"Table", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"einsteinTensorUpIndexMatrix0", "[", 
          RowBox[{"[", 
           RowBox[{"i0", ",", " ", "j0"}], "]"}], "]"}], " ", "=", " ", 
         RowBox[{"Normal", "[", 
          RowBox[{"Series", "[", 
           RowBox[{
            RowBox[{"Sum", "[", 
             RowBox[{
              RowBox[{
               RowBox[{"metricUpIndexMatrix", "[", 
                RowBox[{"[", 
                 RowBox[{"i0", ",", " ", "k0"}], "]"}], "]"}], "*", 
               RowBox[{"metricUpIndexMatrix", "[", 
                RowBox[{"[", 
                 RowBox[{"j0", ",", " ", "l0"}], "]"}], "]"}], "*", 
               RowBox[{"einsteinTensorMatrix", "[", 
                RowBox[{"[", 
                 RowBox[{"k0", ",", " ", "l0"}], "]"}], "]"}]}], ",", " ", 
              RowBox[{"{", 
               RowBox[{"k0", ",", " ", "1", ",", " ", "dim"}], "}"}], ",", 
              " ", 
              RowBox[{"{", 
               RowBox[{"l0", ",", " ", "1", ",", " ", "dim"}], "}"}]}], "]"}],
             ",", " ", 
            RowBox[{"{", 
             RowBox[{"\[Epsilon]1", ",", " ", "0", ",", " ", "order"}], 
             "}"}]}], "]"}], "]"}]}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"i0", ",", " ", "dim"}], "}"}], ",", " ", 
        RowBox[{"{", 
         RowBox[{"j0", ",", " ", "dim"}], "}"}]}], "]"}], ";", "\n", "\t", 
      RowBox[{
      "einsteinTensorUpIndexMatrix", " ", "=", " ", 
       "einsteinTensorUpIndexMatrix0"}], ";", "\n", "\t", 
      "einsteinTensorUpIndexMatrix0"}]}], "]"}]}]}]], "Code",
 CellChangeTimes->{
  3.6133453215536547`*^9, 3.613345914542457*^9, {3.61334662734618*^9, 
   3.613346714807632*^9}, {3.6133472549802523`*^9, 3.613347271761832*^9}}]
},
WindowSize->{1920, 1031},
WindowMargins->{{0, Automatic}, {Automatic, 0}},
Magnification:>1.5 Inherited,
FrontEndVersion->"9.0 for Linux x86 (64-bit) (February 7, 2013)",
StyleDefinitions->"Default.nb"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[557, 20, 1456, 28, 385, "Code"],
Cell[2016, 50, 402, 9, 133, "Code"],
Cell[2421, 61, 1279, 31, 273, "Code"],
Cell[3703, 94, 2950, 71, 245, "Code"],
Cell[6656, 167, 3562, 85, 329, "Code"],
Cell[10221, 254, 3957, 96, 273, "Code"],
Cell[14181, 352, 2961, 74, 301, "Code"],
Cell[17145, 428, 2107, 53, 273, "Code"],
Cell[19255, 483, 1981, 49, 273, "Code"],
Cell[21239, 534, 2275, 57, 273, "Code"],
Cell[23517, 593, 2452, 60, 273, "Code"]
}
]
*)

(* End of internal cache information *)
