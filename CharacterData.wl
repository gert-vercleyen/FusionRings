(* ::Package:: *)

BeginPackage["FusionRings`CharacterData`"];

FusionRingCharacters::usage = 
	"FusionRingCharacters[r], with r a commutative ring, returns a symbolic character table of r or Missing[] if no symbolic form was found.";
FRC::usage = 
	"Shorthand for FusionRingCharacters";
NFusionRingCharacters::usage =
	"NFusionRingCharacters[r], with r a commutative ring, returns a machine precision numeric character table of r.";
NFRC::usage =
	"Shorthand for NFusionRingCharacters.";

Needs["FusionRings`"];


Begin["Private`"]

importDirectory = Quiet[ 
	Check[ SetDirectory @ DirectoryName @ $InputFileName,    (* If not using notebook interface *)
	SetDirectory @ NotebookDirectory[]],SetDirectory::fstr   (* If using notebook interface *)
];

AllFormalCodes = FormalCode /@ FRL;

PrintTemporary["Importing symbolic characters"];
aCharTabs = FusionRings`Private`optimizedImport[ "FusionRingSymCharacters", importDirectory ];

PrintTemporary["Importing numeric characters"];
aNCharTabs = FusionRings`Private`optimizedImport[ "FusionRingNCharacters", importDirectory ];

FusionRingCharacters[ ring_FusionRing?FusionRingQ ] :=
	aCharTabs[ FormalCode[ ring ] ]; 

FRC[ ring_FusionRing?FusionRingQ ] :=
	FusionRingCharacters[ ring ];
	
NFusionRingCharacters[ ring_FusionRing?FusionRingQ ] := 
	aNCharTabs[ FormalCode[ring] ];

NFRC[ ring_FusionRing?FusionRingQ ] :=
	NFusionRingCharacters[ ring ];


End[]

EndPackage[]
