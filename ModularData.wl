(* ::Package:: *)

(* ::Package:: *)

BeginPackage["FusionRings`ModularData`"];

SMatrices::usage = 
	"SMatrices[ring] returns a list of S matrices of the ring, if it has one, and an empty list otherwise.";
SM::usage = 
	"Shorthand for SMatrices.";
TwistAngles::usage =
	"TwistAngles[ring] returns a vector of rational numbers denoting the angles \[Theta] in the expressions Exp[ 2 \[Pi] \[ImaginaryI] \[Theta] ] that appear in the T-matrix of the ring."
TA::usage = 
	"Shorthand for TwistMatrices.";
ModularData::usage = 
	"ModularData[ring] returns a list of associations <| \"SMatrix\" -> Si, \"TwistAngles\" -> \[CapitalTheta]i |>, where the Si are the S matrices of the ring and the \[CapitalTheta]i lists of twistangles for for which the corresponding T-matrix obeys (ST\!\(\*SuperscriptBox[\()\), \(3\)]\) == \!\(\*SuperscriptBox[\(\[Lambda]S\), \(2\)]\) with \[Lambda] a non-zero complex number. If there are no compatible T-matrices for any S-matrix an empty list is returned.";
MD::usage = 
	"Shorthand for ModularData.";

Needs["FusionRings`"];

Begin["Private`"]

importDirectory = Quiet[ 
	Check[ SetDirectory @ DirectoryName @ $InputFileName,    (* If not using notebook interface *)
	SetDirectory @ NotebookDirectory[]],SetDirectory::fstr   (* If using notebook interface *)
];

PrintTemporary["Importing S-matrices"];
aSMatrices = FusionRings`Private`optimizedImport[ "SMatrices", importDirectory ];

PrintTemporary["Importing Twist Angles"];
aTwistAngles = FusionRings`Private`optimizedImport[ "TwistAngles", importDirectory ];

PrintTemporary["Importing S-T-Pairs"];
aModularDatum = FusionRings`Private`optimizedImport[ "ModData", importDirectory ];

SetAttributes[ SMatrices, Listable];
SMatrices[ ring_FusionRing?FusionRingQ ] := With[ {
	s = aSMatrices[ FormalCode[ ring ] ] },
	If[ 
		Head[ s ] === Missing,
		{},
		s
	]
];

SM[ ring_FusionRing?FusionRingQ ] :=
	SMatrices[ ring ];

SetAttributes[ TwistAngles, Listable];
TwistAngles[ ring_FusionRing?FusionRingQ ] := 
	aTwistAngles[ FormalCode[ ring ] ];
	
TA[ ring_FusionRing?FusionRingQ ] :=
	TwistAngles[ ring ];
	
SetAttributes[ ModularData, Listable];
ModularData[ ring_FusionRing?FusionRingQ ] :=  With[ {
	data = aModularDatum[ FormalCode[ ring ] ]},
	If[ 
		Head[ data ] === Missing,
		{},
		data
	]
];

MD[ ring_FusionRing?FusionRingQ ] :=
	ModularData[ ring ];


End[]

EndPackage[]
