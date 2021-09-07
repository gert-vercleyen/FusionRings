(* ::Package:: *)

(* ::Section:: *)
(*Begin Package*)


BeginPackage["FusionRings`"];
(* The structure of the FusionRing objects and the means of storing and using information
   contained in FusionRing has been greatly inspired by the following post on stackexchange
   https://mathematica.stackexchange.com/questions/213618/implement-abstract-algebraic-structure. *)

Print @Block[
{textStyle =Style[ #,  FontSize->12,FontFamily->"Helvetica Neue" ]&,
boldStyle = Style[ #, FontSize->12,FontFamily->"Helvetica Neue", Bold ]&,
codeStyle = Style[ #,  FontSize->12,FontFamily->"Source Code Pro" ]& },
Panel[Apply[StringJoin,ToString[#,StandardForm]&/@#]&@
{
Style[ "FusionRings Package\n" ,FontSize->24,FontFamily->"Helvetica Neue"],

boldStyle[ "Authors: " ],textStyle[ "Gert Vercleyen, Joost Slingerland\n" ],

boldStyle[ "Last Revision: "],textStyle[ "02/08/2021\n" ],
textStyle[ "This package is designed to explore and calculate properties of fusion rings in a user friendly manner. It contains an extensive list fusion rings: "
],
codeStyle[ "FusionRingList" ], 
textStyle[ " (or " ],
codeStyle[ "FRL)"],
textStyle[", and many functions for working with fusion rings. Evaluate "],codeStyle["?FusionRings`*"],
textStyle[" for a full list and click on a name for more information about that function.\nModular data of fusion rings can be obtained by evaluating "],
codeStyle["<<FusionRings`ModularData`"],
textStyle[" and character tables can be obtained by evaluating "],
codeStyle["<<FusionRings`CharacterData`.\n" ],
textStyle["All symbolic data might be simplified much further and you may do so at own risk. Note that functions such as Simplify do not necessarily preserve numerical value due the multivaluedness of complex powers.\nThis package is released under the MIT license (see LICENSE file in top directory) and for professional use such as in academia the authors kindly ask to cite the related article on arxiv: <ARXIV CODE>."]
}]
]


(* ::Subsection:: *)
(*Usage and Error Messages*)


(* ::Subsubsection:: *)
(*Initialization of fusion rings *)


FusionRings::usage = 
	"This package contains a List of fusion rings and several useful tools for working with fusion rings. It also adds formatting tools and the possibility to perform calculations with elements of fusion rings without the need for cumbersome notation.";
FusionRing::usage = 
	"FusionRing[ \"MultiplicationTable\" -> multTab ] initializes a fusion ring based on the given multiplication table multTab. Extra options include\n(i) \"ElementsName\" -> s which gives all elements an indexed name with String or Symbol s, \n(ii) \"ElementNames\" -> elNames where elNames is a list of Strings, Integers or Symbols representing the names of the different elements (giving this option overwrites the \"ElementsName\" option, \n(iii) \"Names\" -> names where names is a list of String's representing the possible names of the fusion ring.";
FusionRingZn::usage = 
	"FusionRingZn[n] returns the group fusion ring \!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \(n\)]\)";
FusionRingSU2k::usage = 
	"FusionRingSU2k[k] returns the fusion ring \!\(\*
StyleBox[\"SU\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"2\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[SubscriptBox[\")\", \"k\"],\nFontWeight->\"Bold\"]\)";
FusionRingPSU2k::usage = 
	"FusionRingPSU2k[k] returns the fusion ring \!\(\*
StyleBox[\"PSU\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"2\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[SubscriptBox[\")\", \"k\"],\nFontWeight->\"Bold\"]\)";
FusionRingFromGroup::usage = 
	"FusionRingFromGroup[g], where g is one of the standard built-in groups, returns a fusion ring whose multiplication matches that of Group. Its name will be set to that of the group if the group is a PermutationGroup, SymmetricGroup, AlternatingGroup, CyclicGroup, DihedralGroup or AbelianGroup, or will be the value given by the option \"Names\".
FusionRingFromGroup[multtable], where multtable is the cayley table of a group, returns a fusion ring whose multiplication matches that of the multiplication table from the group.";
FusionRingHI::usage = 
	"FusionRingHI[g] returns the Haagerup-Izumi fusion ring associated to the built-in abelian group g. FusionRingHI[multtable] returns the Haagerup-Izumi fusion ring associated to the group with multiplication table multtable.";
FusionRingTY::usage =
  "FusionRingTY[g] returns the Tambara-Yamagami fusion ring associated to the built-in abelian group g. FusionRingTY[multtable] returns the Tambara-Yamagami fusion ring associated to the group with multiplication table multtable.";


(* ERROR MESSAGES *)
FusionRing::nomulttable = 
	"No multiplication data is provided so no ring could be initialized.";
FusionRing::wrongdim = 
	"Wrong dimensions: `1`. The dimensions of the multiplication table should be {m,m,m}, with m the amount of particles.";
FusionRing::notassociative = 
	"Multiplication table does not correspond to associative multiplication.";
FusionRing::nounit = 
	"The first element in the multiplication table is not a unit element.";
FusionRing::noinverse = 
	"The multiplication table contains particles with no antiparticle.";
FusionRing::multipleinverse = 
	"The multiplication table contains particles with multiple antiparticles.";
FusionRing::badcoeff = 
	"The multiplication table contains arguments that are not positive integers.";
FusionRing::elnameslength = 
	"Length of list of names `1` is not equal to amount of generators `2` of the fusion ring.";
FusionRing::elnamesdifferentheads = 
	"The elements of list `1` should have equal Head's.";
FusionRing::elnameswrongheads =
	"The elements of list `1` should have Head Integer, String, or Symbol.";
FusionRing::badargnames = 
	"OptionValue `1` of \"Names\" should be a String or a list of Strings.";
FusionRingTY::notgrouptable =
  "The multiplication table `1` must be a group multiplication table.";
FusionRingHI::nonsymmulttab =
  "The multiplication table `1` must be symmetric.";
FusionRingHI::notgrouptable =
  "The multiplication table `1` must be a group multiplication table.";


(* ::Subsubsection::Closed:: *)
(*Properties of fusion rings*)


FusionRingQ::usage = 
	"FusionRingQ[ring] returns True if ring is a FusionRing and False otherwise.";
FusionElement::usage = 
	"FusionElement[ring, i] represents a symbolic form of the i'th generator of the fusion ring r. If no \"ElementsName\" option is set, FusionElement[ring, i] will be formated as \!\(\*SubscriptBox[\(\[Psi]\), \(i\)]\). If the \"ElementsName\" option is set to \"string\" then the FusionElement[ring,i] will be formatted as \!\(\*SubscriptBox[\(string\), \(i\)]\). If the option \"ElementNames\" is set then FusionElement[ring,i] will be formatted by the i'th entry of ring[\"ElementNames\"]";
FusionProduct::usage = 
	"FusionProduct[ring, {i,j}] returns a sum of FusionElements that equals the fusion product of FusionElements i and j.";
MultiplicationTable::usage = 
	"MultiplicationTable[ring] returns a triple nested list l where l[[a,b,c]] is the structure constant N_{a,b}^c. Upon initializing a fusion ring it is necessary to provide the option \"MultiplicationTable\"->table where the table must represent the multiplication table of a unital, associative fusion ring with unique inverses.";
MT::usage = 
	"MT is shorthand for MultiplicationTable.";
SymbolicMultiplicationTable::usage = 
	"SymbolicMultiplicationTable[ring] returns a table l where l[[a,b]] equals fusion.";
SMT::usage = 
	"SMT is shorthand for SymbolicMultiplicationTable.";
Names::usage = 
	"Names[ring] returns a list of possible names of the fusion ring. The possible names of a fusion ring can set upon initialization by adding the option \"Names\"->{...} or afterwards by issuing ring[\"Names\"] = {...}";
ElementsName::usage = 
	"ElementsName[ring] returns a string that is used as an indexed variable for naming the generators of the fusion ring. Indexed names are only used for formatting the elements during calculations. To access the element of an indexed name, use ring[[index]] instead. This name can be given as an option \"ElementsName\"->\"...\" to FusionRing or added later via ring[\"ElementsName\"] = \"...\".";
ElementNames::usage = 
	"ElementNames[ring] returns a list of strings denoting the names of the current generators of the fusion ring. These will be used to label the elements of the ring during calculations and can also be used to access the elements via ring[[el]], where el can either be the name as a string or as a symbol. ElementNames can be given as an option \"ElementNames\" -> stringlist to FusionRing upon initialization or can be added later via ring[\"ElementNames\"] = stringlist. The format required is a list of strings with the same length as the amount of generators.";
GroupQ::usage = 
	"GroupQ[ring] returns True if the multiplication table comes from a finite group.";
GQ::usage = 
	"Shorthand for GroupQ.";
Multiplicity::usage = 
	"Multiplicity[ring] returns and integer that denotes the highest structure constant.";
Mult::usage = 
	"MC is shorthand for Multiplicity.";
NNonZeroStructureConstants::usage = 
	"NNonZeroStructureConstants[ring] returns an Integer that denotes the amount of nonzero structure constants.";
NNZSC::usage = 
	"NNZC is shorthand for NNonZeroStructureConstants.";
NonZeroStructureConstants::usage = 
	"NonZeroStructureConstants[ring] returns a list of triples of indices for which the structure constants are non-zero";
NZSC::usage = 
	"NZC is shorthand for NonZeroStructureConstants.";
Rank::usage = 
	"Rank[ring] returns the amount of generators (particles) of the fusion ring.";
CommutativeQ::usage = 
	"CommutativeQ[ring] returns True if ring is commutative and False otherwise.";
CQ::usage = 
	"CQ is shorthand for CommutativeQ.";
MultiplicityFreeQ::usage = 
	"MultiplicityFreeQ[ring] returns True if ring has no structure constants bigger than 1.";
MFQ::usage =
	"MFQ is shorthand for MultiplicityFreeQ.";
NSelfDual::usage = 
	"NSelfDual[ring] returns the amount of particles that are their own antiparticle. This includes the vacuum particle";
NSD::usage = 
	"NSD is shorthand for NSelfDual.";
NNonSelfDual::usage = 
	"NNonSelfDual[ring] returns the amount of particles that are not their own antiparticle";
NNSD::usage = 
	"NNSD is shorthand for NNonSelfDual.";
NSelfDualNonSelfDual::usage = 
	"NSelfDualNonSelfDual[ring] returns a tuple {sd,nsd} where sd is the amount of particles that are their own antiparticle (including the vacuum particle and nsd is the amount of particles that are not their own antiparticle";
NSDNSD::usage = 
	"NSDNSD is shorthand for NSelfDualNonSelfDual.";
AntiparticleMatrix::usage = 
	"AntiparticleMatrix[ring] returns a matrix that maps each particle to its antiparticle.";
AM::usage = 
	"AM is shorthand for AntiparticleMatrix.";
QuantumDimensions::usage = 
	"QuantumDimensions[Ring] returns the list of quantum dimensions of each generator in the same order as that of the generators";
QD::usage = 
	"QD is shorthand for QuantumDimensions.";
TotalQuantumDimensionSquared::usage =
	"TotalQuantumDimensionSquared[Ring] returns the sum of the squares of the quantum dimensions of the generators of Ring";
TQDS::usage =
	"TQDS is shorthand for TotalQuantumDimensionSquared.";
Barcode::usage = 
	"Barcode[ring] returns a number associated to the set of rings obtained by permuting the elements of ring. 
It equals the maximum of the numbers obtained flattening each multiplication table and interpreting the lists of integers as digits of an integer in base Multiplicity[ring] + 1. It is therefore an invariant under permutation of the elements of a ring and for rings with equal multiplicity it uniquely determines the structure of a ring.";
FormalCode::usage =
	"FormalCode[ring] returns a 4-tuple that uniquely classifies the ring. The first 3 numbers are the Rank, Multiplicity, and Number of non-seldual particles, while the 4th is the position in the list of rings with common first 3 numbers, sorted by amount of nonzero structure constants and Barcode.";
FC::usage = 
	"Shorthand for FormalCode";
ConjugateCharge::usage = 
	"ConjugateCharge[ring] returns a function that maps an integer index to the dual index.";
CC::usage =
	"Shorthand for ConjugateCharge";
FusionRingAutomorphisms::usage = 
	"FusionRingAutomorphisms[ring] returns a list of permutation vectors that leaves the multiplication table of ring invariant.";
FRA::usage =
	"Shorthand for FusionRingAuthomorphisms.";

(*ToWikiTable::usage = 
	"ToWikiTable[table] returns a string that formats the table into the default format for a 'wikitable sortable' table on mediawiki.";*)


(* ::Subsubsection:: *)
(*Operations on fusion rings*)


PermutedRing::usage = 
	"PermutedRing[ring,permutationvec] returns a ring with multiplication table obtained after permuting the labels of ring according to the permutation induced by the permutation vector permutationvec.
PermutedRing[ring,cycles] returns a ring with multiplication table obtained after permuting the labels of ring according to the permutation induced by cycles.";
(* ERROR MESSAGES *)
PermutedRing::invalidpermutation = 
	"Permutation vector `1` should either be of length `2` and contain all entries  in the range 1...`2` exactly once with a 1 on the first position or should contain all entries in the range 2...`2` exactly once";
SortedRing::usage =
	"SortedRing[ring] returns a ring whose generators are ordered according by increasing quantum dimension. SortedRing has the option \"SortBy\" that allows to choose a different order from the following: \"Selfdual-Conjugates\" or \"Conjugates-Selfdual\"";
RenameElements::usage = 
	"RenameElements[ring,elementnames] returns a ring with with elements named after elementnames. Here elementnames is only allowed to be a list of either String's, Integer's or Symbol's and must have a length equal to the rank of the ring.";
AddName::usage = 
	"AddName[ring,string] returns a ring where the name string is added to the list of possbile names of ring.";
SetNames::usage = 
	"SetNames[ring,stringlist] returns a ring for which the names are now stringlist.";


(* ::Subsubsection::Closed:: *)
(*Combining, Decomposing and comparing fusion rings*)


DirectProduct::usage = 
	"DirectProduct[ring1,ring2] returns the direct ring product of ring1 and ring2.";
EquivalentFusionRingQ::usage = 
	"EquivalentFusionRingQ[ring1,ring2] returns True if the elements of ring1 are a relabeling of the elements of ring2 and False otherwise.";
EFQ::usage = 
	"Shorthand for EquivalentFusionRingQ.";
(*WhichPermutation::usage = 
	"WhichPermutation[ring1,ring2] returns a permutation vector v such that PermutedRing[ring1,v] equals ring2";*)
EquivalentFusionRings::usage = 
	"EquivalentFusionRings[ring] returns a list of all rings r for which there exists a permutation vector \[Sigma] with \[Sigma][[1]] == 1 and r equals PermutedRing[ring,\[Sigma]].";
WhichDecompositions::usage = 
	"WhichDecompositions[ring] returns a list of lists of fusion rings whose direct product is a fusion ring isomorphic to ring.";
WD::usage = 
	"Shorthand for WhichDecompositions.";
SubFusionRings::usage = 
	"SubFusionRings[r] returns a list of tuples { s[i], ring[i] } where s[i] is a list of indices such that the restriction of the fusion ring r to the elements with those indices gives the fusion ring r[i].";
InjectionForm::usage = 
	"InjectionForm[ ring, subring ] returns a fusion ring homomorphism as an association that maps each element of subring to a corresponding element of ring.";


(* ::Subsubsection::Closed:: *)
(*Working with elements*)


(* ERROR MESSAGES *)
FusionRing::elnotfound = 
	"`1` is not a known name of an element";
FusionRing::eloutofbounds =
	"Particle number `1` does not belong to any particle in the ring";


(* ::Subsubsection::Closed:: *)
(*Dataset*)


FusionRingList::usage = 
	"FusionRingList is a list of all saved FusionRing objects.";
FRL::usage =
	"Shorthand for FusionRingList.";
AllFusionRingsQ::usage =
	"AllFusionRingsAvailableQ[ r, m ] returns true if FusionRingList contains all fusion rings of rank r and multiplicity m, and False otherwise.";
AFRQ::usage = 
	"Shorthand for AllFusionRingsAvailableQ.";	
FusionRingByCode::usage =
	"FusionRingByCode[fourtuple] returns the fusion ring with formal code equal to fourtuple.";
FRBC::usage =
	"Shorthand for FusionRingByCode."


(* ::Subsubsection::Closed:: *)
(*Exporting data*)


ToWikiTable::usage = 
	"Converts a Mathematica table to a string that represents the ttable in MediaWiki format.";


(* ::Subsection:: *)
(*Options & Packages*)


Options[ FusionRing ] = {
  "MultiplicationTable" -> Missing[], 
	"Names"               -> Missing[],
	"ElementsName"        -> Missing[],
	"ElementNames"        -> Missing[],
	"Barcode"             -> Missing[],
	"FormalParameters"    -> Missing[],
	"DirectProductDecompositions" -> Missing[],
	"SubFusionRings"      -> Missing[]
};

SetOptions[ Root, ExactRootIsolation -> True ];


(* ::Section:: *)
(*Begin `Private` Context*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Function definitions*)


(* ::Subsubsection:: *)
(*Initialization of fusion rings*)


(* Functions to check for errors *)
ProperStructureConstantsQ[ multTable_ ] := 
	And @@ Map[ IntegerQ[#] && # >= 0 &, Flatten @ multTable ];
	
ProperDimensionsQ[ multTable_ ] := 
	With[{ d = Dimensions @ multTable },
		TrueQ[ Equal @@ d ] && TrueQ[ Length[ d ] == 3 ]	
	];
	
HasUnitQ[ multTable_ ] := 
	With[{ i = IdentityMatrix[ Length @ multTable ] },
		multTable[[ 1, All, All ]] === multTable[[ All, 1, All ]] === i
	];
	
UniqueInverseQ[ multTable_ ] := 
	With[{apMatrix = multTable[[All,All,1]]},
		And @@ 
			Join[ 
				Map[ Count[ #, x_/; x > 0 ] < 2 &, apMatrix ],
				Map[ Count[ #, x_/; x > 0 ] < 2 &, Transpose @ apMatrix ]
			]
	];

AllInversesQ[ multTable_ ] := 
	With[ { apMatrix = multTable[[All,All,1]] },
		And @@ 
			Join[ 
				Map[ Count[ #, x_/; x > 0 ] > 0 &, apMatrix ],
				Map[ Count[ #, x_/; x > 0 ] > 0 &, Transpose @ apMatrix ]
			]
	];

AssociativityMatrixCompiled = 
	Compile[
		{{multTable,_Integer,3}},
		Table[ 
		  multTable[[a,b,1;;Length[multTable]]] . multTable[[1;;Length[multTable],c,d]] 
		- multTable[[a,1;;Length[multTable],d]] . multTable[[b,c,1;;Length[multTable]]],
		{a,Length[multTable]}, {b,Length[multTable]}, {c,Length[multTable]}, {d,Length[multTable]} ],
		{{zerosTable,_Integer,3}},
		CompilationOptions -> {"ExpressionOptimization"->True},
		"RuntimeOptions" -> "Speed"	
	];
AssociativeQ[ table_ ] := With[ {
	truthTab = AssociativityMatrixCompiled[table]},
	Equal@@Flatten[truthTab] && truthTab[[1,1,1,1]]==0
];

(* All the core data will be stored in an Association for convenience of 
   access. We still want FusionRing to be the head of the object so 
   we store the association inside the function FusionRing, which will 
   only serve as a wrapper and never return a value *)

FusionRing[ ops:OptionsPattern[] ] := 
	FusionRing[ InitializeFusionRing[ ops ] ];
	
(* Special toString function that provides strings of subscripted symbols *)
toString = Function[expr, ToString[expr, StandardForm], {HoldAll, Listable}];

Options[ InitializeFusionRing ] = Options[ FusionRing ];

InitializeFusionRing[ ops:OptionsPattern[] ] := With[{
		multTab = "MultiplicationTable" // OptionValue // Developer`ToPackedArray,
		names   = 
			Which[ 
				# === Missing[], {}, 
				StringQ[#], {#}, 
				Head[#] === List && And @@ StringQ /@ #, #, 
				True, Message[FusionRing::badargnames, # ] 
			]& @ OptionValue["Names"],
		elsName = If[ # =!= Missing[], #, "\[Psi]" ]& @ ("ElementsName" // OptionValue)
		},
		With[ {
			elNames = If[ # =!= Missing[], #, Table[ elsName <>"["<>ToString[i]<>"]", { i, Length @ multTab }]]& @ OptionValue["ElementNames"],
			dualvec = If[ Length[multTab] == 1, {1}, Join[ {1}, multTab[[2;;,2;;,1]] . ( Range[ Length[multTab] - 1 ] + 1 ) ] ]
			},
			Which[
				multTab === None, 
					Message[ FusionRing::nomulttable ],
				!ProperStructureConstantsQ[ multTab ], 
					Message[ FusionRing::badcoeff ],
				!ProperDimensionsQ[ multTab ],
					Message[ FusionRing::wrongdim, Dimensions[ multTab ] ],
				!HasUnitQ[ multTab ],
					Message[ FusionRing::nounit ],
				!UniqueInverseQ[ multTab ],
					Message[ FusionRing::multipleinverse ],
				!AllInversesQ[ multTab ],
					Message[ FusionRing::noinverse ],
				!AssociativeQ[ multTab ],
					Message[ FusionRing::notassociative ],
				elNames =!= None && Length[ elNames ] != Length[ multTab ],   
					Message[ FusionRing::elnameslength, Length[ elNames ], Length[ multTab ] ],
				!( Equal @@ Head /@ elNames),
					Message[ FusionRing::elnamesdifferentheads, elNames ],
				!(MemberQ[{String,Integer,Symbol}, Head @* First @ elNames]),
					Message[ FusionRing::elnameswrongheads, elNames],
				True,
					Association[
						"Barcode" -> OptionValue["Barcode"],
						"DirectProductDecompositions" -> OptionValue["DirectProductDecompositions"],
						"Dual" -> Function[ i, dualvec[[i]] ],
						"ElementNames" -> elNames,
						"ElementsName" -> elsName,
						"FormalParameters" -> OptionValue["FormalParameters"],
						"MultiplicationTable" -> multTab,
						"Names" -> names,
						"SubFusionRings" -> OptionValue["SubFusionRings"]
					]
			]
		]
	];
	

(* Constructors for some common fusion rings *)
(* Z_n *)
FusionRingZn[ n_Integer ] := 
	FusionRing @ Sequence[ 
	Rule[ "MultiplicationTable", Table[ 
		If[ Mod[c-1,n] == Mod[a+b-2,n], 
			1, 
			0 
		], 
	{ a, n }, { b, n }, { c, n } ]],
	Rule[ "Names", {"\!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \("<>ToString[n]<>"\)]\)"} ]
];

(* PSU(2)_k *)
rangePSU2k[ i_Integer, j_Integer, k_Integer ] := 
	Table[ l, { l, Abs[ i - j ], Min[ i + j, 2k - i - j ] , 2 } ];

FusionRingPSU2k[ k_Integer ] := 
	FusionRing @ Sequence[ 
	Rule[ "MultiplicationTable", Table[ 
		If[ MemberQ[ rangePSU2k[ i, j, k ], l ], 
			1, 
			0 
		], 
	{ i, 0, k, 2 }, { j, 0, k, 2 }, { l, 0, k, 2 } ]
	],
	Rule[ "Names" , {"PSU(2\!\(\*SubscriptBox[\()\), \("<>ToString[k]<>"\)]\)"}]
];

(* SU(2)_k *)
rangeSU2k[ i_Integer, j_Integer, k_Integer ] := rangePSU2k[ i, j, k ];
FusionRingSU2k[ k_Integer ] := 
	FusionRing @ Sequence[ 
	Rule[ "MultiplicationTable", Table[ 
		If[ MemberQ[ rangeSU2k[ i, j, k ], l ], 
			1, 
			0 
		], 
	{ i, 0, k }, { j, 0, k }, { l, 0, k } ]
	],
	Rule[ "Names" , {"SU(2\!\(\*SubscriptBox[\()\), \("<>ToString[k]<>"\)]\)"} ]
];

(* Fusion rings from groups *)
(* Code for assigning the correct name *)
GroupName[PermutationGroup[list_]]:= "\!\(\*SubscriptBox[\(G\), \(Perm\)]\)["<>(ToString@StringReplace[ToString[#],{"{"->"(",","->"","}"->")"}]&/@Cases[list,Cycles[x_]->x,\[Infinity]])<>"]"
GroupName[SymmetricGroup[n_]]:= "\!\(\*SubscriptBox[\(S\), \("<>ToString[n]<>"\)]\)";
GroupName[AlternatingGroup[n_]]:= "\!\(\*SubscriptBox[\(A\), \("<>ToString[n]<>"\)]\)";
GroupName[DihedralGroup[n_]]:= "\!\(\*SubscriptBox[\(D\), \("<>ToString[n]<>"\)]\)";
GroupName[CyclicGroup[n_]]:= "\!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \("<>ToString[n]<>"\)]\)";
GroupName[AbelianGroup[list_]]:= StringJoin@@Riffle[ Table["\!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \("<>ToString[i]<>"\)]\)",{i,list}],"\[Cross]"]
GroupName[_] := "";

(* Code for checking whether the multiplication table is that of a group *)
Group::invalidrangesmulttab =
  "Not every row and column in `1` contains numbers from 1 to `2`.";
Group::nouniqueinverse =
  "Not every element in `1` has a unique inverse.";
Group::nonassociativemulttab =
  "The multiplication defined by `1` is not associative.";
Group::noninvertibleelement =
  "Not every element is invertible.";
GroupTableQ[ tab_?MatrixQ ] := With[{
	m = tab[[#1,#2]]&,
	n = tab // Length},
  Which[
		Not[ And @@ Map[ Sort[#] == Range[n]&, tab ~ Join ~ Transpose[ tab] ] ],
			Message[ Group::invalidrangesmulttab, tab, n]; False,
		Not[ Times @@ Count[1] /@ tab == 1],
			Message[ Group::nouniqueinverse, tab ]; False,
		Not[ And @@ Flatten @ Array[ m[#1,m[#2,#3]] == m[m[#1,#2],#3]&, {n,n,n} ] ],
			Message[ Group::nonassociativemulttab, tab ]; False,
		Not[ tab[[;;,1]] == tab[[1,;;]] == Range[n] ],
			Message[ Group::noninveribleelement ]; False,
		True,
			True
	]
];

Options[FusionRingFromGroup] := { "Names" -> {""}};
FusionRingFromGroup[ table_?MatrixQ, OptionsPattern[] ] := With[ {
	n = Length[ table ]},
	If[ GroupTableQ[table],
		FusionRing @ Sequence[
			Rule[
				"MultiplicationTable",
				Table[
					If[ k == table[[i,j]], 1 , 0]
				, { i, n }, { j, n }, { k, n }]
			],
			Rule[
				"Names",
				OptionValue["Names"]
			]
		]
	]
];
	
FusionRingFromGroup[ group_, opts:OptionsPattern[] ] := 
	FusionRingFromGroup[ 
		GroupMultiplicationTable @ group, 
		If[ OptionValue[ "Names" ] == { "" },
			"Names" -> { GroupName @ group },
			OptionValue[ "Names" ]
		]
	];
	
(* Haagerup-Izumi *)
Options[FusionRingHI] = {"Names" -> {} };
FusionRingHI[ tab_?MatrixQ, OptionsPattern[] ] := With[{
	n = tab // Length,
	inv = tab // Position[ #, 1 ]& // Transpose // Last
	},
	Which[ 
		!GroupTableQ[ tab ], Message[ FusionRingHI::nongrouptable, tab ] ; Return[],
		!SymmetricMatrixQ[ tab ], Message[ FusionRingHI::nonsymmulttab, tab ],
		True,
		If[
			OptionValue["Names"] === {},
			FusionRing @ Sequence[
			Rule[ "MultiplicationTable", Table[
				Which[
					i <= n && j <= n, Table[ If[ k == tab[[ i, j ]], 1, 0 ], { k, 2n } ],
					i <= n && j >  n, Table[ If[ k == n + tab[[ i, j - n ]], 1, 0 ], { k, 2n } ],
					i > n  && j <= n, Table[ If[ k == n + tab[[ inv[[ j ]], i - n ]], 1, 0 ], { k, 2n } ],
					i > n  && j >  n, Table[ If[ k == tab[[ i - n, inv[[ j - n ]] ]] || k > n, 1, 0 ], { k, 2n } ]
				],
				{ i, 2n }, { j, 2n } ]
			]
			],
			FusionRing @ Sequence[
			Rule[ "MultiplicationTable", Table[
				Which[
					i <= n && j <= n, Table[ If[ k == tab[[ i, j ]], 1, 0 ], { k, 2n } ],
					i <= n && j >  n, Table[ If[ k == n + tab[[ i, j - n ]], 1, 0 ], { k, 2n } ],
					i > n  && j <= n, Table[ If[ k == n + tab[[ inv[[ j ]], i - n ]], 1, 0 ], { k, 2n } ],
					i > n  && j >  n, Table[ If[ k == tab[[ i - n, inv[[ j - n ]] ]] || k > n, 1, 0 ], { k, 2n } ]
				],
				{ i, 2n }, { j, 2n } ]
			],
			Rule[
				"Names",
				OptionValue["Names"]
			]
			]
		]
	]
];

FusionRingHI[ group_, OptionsPattern[] ] := 
	FusionRingHI[
		GroupMultiplicationTable @ group, 
		If[ OptionValue["Names"] != {""},
			"Names" -> OptionValue["Names"],
			If[ GroupName[group] != "",
				"Names" -> { "HI(" <> GroupName[group] <> ")" },
				"Names" -> { "" }
			]
		]
	];

(* Tambara Yamagami *)
Options[FusionRingTY] = {"Names" -> {} };
FusionRingTY[ tab_?MatrixQ, OptionsPattern[] ] := With[{
	n = tab // Length },
	If[
		!GroupTableQ[ tab ], Message[ FusionRingTY::notgrouptable, tab ]; Return[],
		If[
			OptionValue["Names"] === {},
			(* THEN *)
			FusionRing @ Sequence[
			Rule[ "MultiplicationTable", Table[
				Which[
					i <= n && j <= n, Table[ If[ k == tab[[ i, j ]], 1, 0 ], { k, n + 1 } ],
					i <= n && j >  n, Table[ If[ k == n + 1, 1, 0 ],         { k, n + 1 } ],
					i >  n && j <= n, Table[ If[ k == n + 1, 1, 0 ],         { k, n + 1 } ],
					i >  n && j >  n, Table[ If[ k <= n, 1, 0 ],             { k, n + 1 } ]
				],
				{ i, n + 1 }, { j, n + 1 } ]
				]
			],
			(* ELSE *)
			FusionRing @ Sequence[
			Rule[ "MultiplicationTable", Table[
				Which[
					i <= n && j <= n, Table[ If[ k == tab[[ i, j ]], 1, 0 ], { k, n + 1 } ],
					i <= n && j >  n, Table[ If[ k == n + 1, 1, 0 ],         { k, n + 1 } ],
					i >  n && j <= n, Table[ If[ k == n + 1, 1, 0 ],         { k, n + 1 } ],
					i >  n && j >  n, Table[ If[ k <= n, 1, 0 ],             { k, n + 1 } ]
				],
				{ i, n + 1 }, { j, n + 1 } ]
				],
			Rule[
				"Names",
				OptionValue["Names"]
			]
			]
		]			
	]
];

FusionRingTY[ group_, OptionsPattern[] ] :=
	FusionRingTY[
		GroupMultiplicationTable @ group,
		If[ OptionValue["Names"] != {""},
			"Names" -> OptionValue["Names"],
			If[ GroupName[group] != "",
				"Names" -> { "TY(" <> GroupName[group] <> ")" },
				"Names" -> { "" }
			]
		]
	];


(* ::Subsubsection:: *)
(*Properties of fusion rings *)


(* FusionRingQ will be implemented such way that we don't always need to check whether
   the ring is a fusion ring or not. *)

validateFusionRing[ r_Association ] := With[{ 
	mtab = r["MultiplicationTable"] },
	TrueQ[
		And[
			ProperStructureConstantsQ @ #,
			ProperDimensionsQ @ #,
			HasUnitQ @ #,
			UniqueInverseQ @ #,
			AllInversesQ @ #,
			AssociativeQ @ #
		]
	]& @ mtab
];
	 
FusionRing[ r_Association ]?NotFusionRingQ :=
	(System`Private`HoldSetValid[ FusionRing[ r ] ])/; validateFusionRing[ r ];

FusionRingQ[ r_FusionRing ] := 
	System`Private`HoldValidQ[ r ];
FusionRingQ[ _ ] := 
	False;
FusionRingQ[ s_Symbol ] := 
	(Head[s] === FusionRing && FusionRingQ[Evaluate[s]]);
SetAttributes[ FusionRingQ, HoldFirst ];

NotFusionRingQ[ r_ ] := Not @ FusionRingQ[r];
SetAttributes[ NotFusionRingQ, HoldFirst ];



(* Access elements of the fusion ring as if it were an association. *)
   
FusionRing[ r_ ]?FusionRingQ[ k_ ] := Lookup[ r, k ];

(* Define getter functions for the fusion ring. Since system symbols
   are protected and we don't want to alter them unless strictly neccessary 
   we use TagSetDelayed ( /: lhs := rhs ) to pattern match FusionRing objects 
   within expressions and replace them with our own definitions. *)
   
SetAttributes[ MultiplicationTable, Listable ];
FusionRing /: MultiplicationTable[ r_FusionRing?FusionRingQ ] := 
	r["MultiplicationTable"];

SetAttributes[ MT, Listable ];
FusionRing /: MT[ r_FusionRing?FusionRingQ ] :=
	MultiplicationTable[ r ];
	
SetAttributes[ SymbolicMultiplicationTable, Listable ];
FusionRing /: SymbolicMultiplicationTable[ r_FusionRing?FusionRingQ ] := 
	Table[ FusionProduct[ r, {a,b} ], {a, Rank[r]}, {b,Rank[r]} ];

SetAttributes[ SMT, Listable ];
FusionRing /: SMT[ r_FusionRing?FusionRingQ ] :=
	SymbolicMultiplicationTable[ r ];

SetAttributes[ Names, Listable ];
FusionRing /: Names[ r_FusionRing?FusionRingQ ] := 
	r["Names"];

SetAttributes[ ElementsName, Listable ];
FusionRing /: ElementsName[ r_FusionRing?FusionRingQ ] := 
	r["ElementsName"];

SetAttributes[ ElementNames, Listable ];
FusionRing /: ElementNames[ r_FusionRing?FusionRingQ ] := 
	r["ElementNames"];

SetAttributes[ AntiparticleMatrix, Listable ];
FusionRing /: AntiparticleMatrix[ r_FusionRing?FusionRingQ ] := 
	MT[r][[All,All,1]];

SetAttributes[ AM, Listable ];
FusionRing /: AM[ r_FusionRing?FusionRingQ ] :=
	AntiparticleMatrix[ r ];

SetAttributes[ CommutativeQ, Listable ];
FusionRing /: CommutativeQ[ r_FusionRing?FusionRingQ ] := 
	TrueQ[ Transpose[ MT[r] ] == MT[r] ];

SetAttributes[ CQ, Listable ];
FusionRing /: CQ[ r_FusionRing?FusionRingQ ] :=
	CommutativeQ[ r ];

SetAttributes[ Multiplicity, Listable ];
FusionRing /: Multiplicity[ r_FusionRing?FusionRingQ ] := 
	Max[ MultiplicationTable[ r ] ] 

SetAttributes[ Mult, Listable ];
FusionRing /: Mult[ r_FusionRing?FusionRingQ ] :=
	Multiplicity[ r ];

SetAttributes[ MultiplicityFreeQ, Listable ];
FusionRing /: MultiplicityFreeQ[ r_FusionRing?FusionRingQ ] := 
	Multiplicity[r] == 1;

SetAttributes[ MFQ, Listable ];
FusionRing /: MFQ[ r_FusionRing?FusionRingQ ] :=
	MultiplicityFreeQ[ r ];
	
SetAttributes[ NonZeroStructureConstants, Listable ];
FusionRing /: NonZeroStructureConstants[ r_FusionRing?FusionRingQ ] := With[{
	range = Range[Rank[r]],
	mt = MT[r]},
	Complement[ Tuples[ {range,range,range} ], Position[ MT[r], 0 ] ]
];

SetAttributes[ NZSC, Listable ];
FusionRing /: NZSC[ r_FusionRing?FusionRingQ ] :=
	NonZeroStructureConstants[ r ];	

SetAttributes[ NNonZeroStructureConstants, Listable ];
FusionRing /: NNonZeroStructureConstants[ r_FusionRing?FusionRingQ ] := 
	Length @ NonZeroStructureConstants[ r ];

SetAttributes[ NNZSC, Listable ];
FusionRing /: NNZSC[ r_FusionRing?FusionRingQ ] :=
	NNonZeroStructureConstants[ r ];

SetAttributes[ Rank, Listable ];
FusionRing /: Rank[ r_FusionRing?FusionRingQ ] := 
	Length @ MultiplicationTable[r];

SetAttributes[ QuantumDimensions, Listable ];
FusionRing /: QuantumDimensions[ r_FusionRing?FusionRingQ ] := 
QuantumDimensions[ r ] = Block[{
	tab = MultiplicationTable[r]},
	RealAbs[ Last[ SortBy[ #, Abs @* N @* Re ] ] ]& /@ Eigenvalues /@ tab
];

SetAttributes[ QD, Listable ];
FusionRing /: QD[ r_FusionRing?FusionRingQ ] :=
	QuantumDimensions[ r ];

SetAttributes[ TotalQuantumDimensionSquared, Listable ];
FusionRing /: TotalQuantumDimensionSquared[ r_FusionRing?FusionRingQ ] :=
	QuantumDimensions[r] . QuantumDimensions[r];

SetAttributes[ TQDS, Listable ];
FusionRing /: TQDS[ r_FusionRing?FusionRingQ ] :=
	TotalQuantumDimensionSquared[ r ];

SetAttributes[ NSelfDual, Listable ];
FusionRing /: NSelfDual[ r_FusionRing?FusionRingQ ] := 
	AntiparticleMatrix[r] // Diagonal // Count[x_/; x != 0 ];

SetAttributes[ NSD, Listable ];
FusionRing /: NSD[ r_FusionRing?FusionRingQ ] :=
	NSelfDual[ r ];

SetAttributes[ NNonSelfDual, Listable ];
FusionRing /: NNonSelfDual[ r_FusionRing?FusionRingQ ] := 
	(Rank[#] - NSelfDual[#])& @ r;

SetAttributes[ NNSD, Listable ];
FusionRing /: NNSD[ r_FusionRing?FusionRingQ ] :=
	NNonSelfDual[ r ];

SetAttributes[ NSelfDualNonSelfDual, Listable ];
FusionRing /: NSelfDualNonSelfDual[ r_FusionRing?FusionRingQ ] := With[{
	nsd = NSelfDual[r]},
	{ nsd, Rank[r] - nsd }	
];

SetAttributes[ NSDNSD, Listable ];
FusionRing /: NSDNSD[ r_FusionRing?FusionRingQ ] :=
	NSelfDualNonSelfDual[ r ];

SetAttributes[ GroupQ, Listable ];
FusionRing /: GroupQ[ r_FusionRing?FusionRingQ ] := 
	Total[ Flatten[ MultiplicationTable[ r ] ] ] == Rank[ r ]^2; 

FusionRing /: GQ[ r_FusionRing?FusionRingQ ] := 
	GroupQ[r];
	
FusionRing /: ConjugateCharge[ r_FusionRing?FusionRingQ ] := 
	r["Dual"];

FusionRing /: CC[ r_FusionRing?FusionRingQ ] := 
	ConjugateCharge[ r ];	
	
FusionElement /: ConjugateCharge[ FusionElement[ R_FusionRing, i_ ] ] := 
	R[[CC[R][i]]];
	
FusionElement /: CC[ e_FusionElement ] :=
	ConjugateCharge[e];
	
SetAttributes[ FormalCode, Listable ];
FusionRing /: FormalCode[ r_FusionRing?FusionRingQ ] :=
	r["FormalParameters"];
	
SetAttributes[ FC, Listable ];
FusionRing /: FC[ r_FusionRing?FusionRingQ ] :=
	FormalCode[ r ];

(* multTabCode assigns a number to a fusion ring based on the multiplicationtable. This 
   number is unique per multiplicationtable but not invariant under permutations of the 
   elements. The function barcode will 
  *)
multTabCode[ ring_FusionRing?FusionRingQ ] := 
multTabCode[ ring ] =
	FromDigits[ Flatten[ MultiplicationTable[ ring ] ] ];

(* If the ring does not have a barcode, either construct a new ring that does have one or never add the info to the
   ring in the first place.
 *)
SetAttributes[ Barcode, Listable ];
FusionRing /: Barcode[ r_FusionRing?FusionRingQ ] := 
	If[ 
		r["Barcode"] =!= Missing[],
		r["Barcode"],
		Module[{
			sRing = SortedRing[ r, "SortBy" -> "Selfdual-Conjugates" ],
			rank = Rank @ r,
			qds,
			permutations},
			qds = Rest @ QuantumDimensions[ sRing ]; (* 1 should be left alone *)
			permutations = Flatten /@ Tuples[ Permutations /@ (GatherBy[ Range[ rank - 1 ], qds[[#]]& ] + 1) ];
			Max[ multTabCode[ PermutedRing[ sRing, # ] ]& /@ permutations] 
		]
	]


(* ::Subsubsection:: *)
(*Operations on fusion rings*)


(* PERMUTING ELEMENTS OF RINGS *)
PermuteMultTab = 
	Compile[
		{{m,_Integer,3},{\[Sigma],_Integer,1}},
		Table[ 
		  m[[ \[Sigma][[a]], \[Sigma][[b]], \[Sigma][[c]] ]],
		{a,Length[m]}, {b,Length[m]}, {c,Length[m]} ],
		{{permTab,_Integer,3}},
		CompilationOptions -> {"ExpressionOptimization"->True},
		"RuntimeOptions" -> "Speed"
	];
	
PermutedRing[ r_FusionRing?FusionRingQ, \[Sigma]_List ] := Module[{  
	multTab = MultiplicationTable[r], 
	m = Rank[r],
	hasUnit = MemberQ[ \[Sigma], 1 ],
	newMultTab},
	If[ Not[ (hasUnit && Sort[\[Sigma]] == Range[m]) || Sort[\[Sigma]] == Range[m-1] + 1 ], 
		Message[ PermutedRing::invalidpermutation, \[Sigma], m ], 
		newMultTab = If[ hasUnit, 
						 PermuteMultTab[ multTab, \[Sigma] ],
						 PermuteMultTab[ multTab, Prepend[ \[Sigma], 1 ] ]
						 ];
		FusionRing @ Sequence[
			"MultiplicationTable" -> newMultTab,
			"Names" -> Names[r],
			"ElementNames" -> ElementNames[r],
			"Barcode"             -> Barcode @ r,
			"FormalParameters"    -> FC @ r,
			"DirectProductDecompositions" -> WhichDecompositions @ r,
			"SubFusionRings"      -> SubFusionRings @ r
		]
	]
];

PermutedRing[ r_FusionRing?FusionRingQ, \[Sigma]_Cycles ] := 
	PermutedRing[ r, PermutationList[ \[Sigma], Rank[r] ] ];

(* SORTING ELEMENTS OF RINGS *)
(* Based on quantumdimensions *)
Options[PermVecQD] = {"Order" -> "Increasing"};
PermVecQD[r_FusionRing?FusionRingQ, OptionsPattern[] ] := With[{
	qds = QuantumDimensions[r],
	range = Range[ Rank[r] - 1] + 1},
	Prepend[
		If[ OptionValue["Order"] == "Increasing",
			Identity,
			Reverse
		] @ SortBy[ range, N[ qds[[#]], 100 ]& ],
		1
	]
]

(* Based on anti-particles *)
Options[PermVecSDConj] = Options[PermVecQD];
PermVecSDConj[r_FusionRing?FusionRingQ, OptionsPattern[]] := Module[{ 
	apmat = AntiparticleMatrix[r],
	qds = QuantumDimensions[r],
	pairs,
	sdpos,
	nsdpos,
	qdSort},
	pairs = DeleteCases[ DeleteDuplicates[ SortBy[ #, Function[ x, N[ qds[[x]] ] ] ]& /@ Position[ apmat, 1 ] ], {1,1} ];
	qdSort[ l_List ] := SortBy[ l, qds[[#]]& ]; 
	
	If[ OptionValue["Order"] == "Increasing",
		Identity,
		Reverse
	] @ 
	Prepend[
		Flatten[
			Join[
				qdSort @ Cases[ pairs, { a_Integer, a_Integer } -> a ], 
				qdSort /@ SortBy[ Cases[ pairs, {a_Integer,b_Integer}/; a != b], Max @ N[ qds[[#]], 100000 ] & ]
			]
		],
	1]
]

(* Function that performs the sorting *)
SetAttributes[ SortedRing, Listable ];
Options[SortedRing] = { "SortBy" -> "QuantumDimensions" };
SortedRing[ r_FusionRing?FusionRingQ, OptionsPattern[] ] := With[{
	permVec = Which[ OptionValue["SortBy"] == "Selfdual-Conjugates", PermVecSDConj[r],
					 OptionValue["SortBy"] == "Conjugates-Selfdual", PermVecSDConj[r]//Reverse,
					 True, PermVecQD[r]] },
	PermutedRing[ r, permVec ]
]


(* Changing information of fusion rings. 
   Everytime you want to change information about a FusionRing you basically create a new fusion ring
   with the desired info added. Upon creation it is checked whether the object is a valid fusion ring 
   so altering information might be a costly operation. 
*)

RenameElements[ r_FusionRing?FusionRingQ, list_ ] := 
	FusionRing @ Sequence[
		"MultiplicationTable" -> MultiplicationTable @ r,
		"ElementsName" -> ElementsName @ r,
		"ElementNames" -> list,
		"Names" -> Names @ r,
		"Barcode"             -> Barcode @ r,
		"FormalParameters"    -> FC @ r,
		"DirectProductDecompositions" -> WhichDecompositions @ r,
		"SubFusionRings"      -> SubFusionRings @ r		
	];
		
AddName[ r_FusionRing?FusionRingQ, s_String ] := 
	FusionRing @ Sequence[
		"MultiplicationTable" -> MultiplicationTable @ r,
		"ElementsName" -> ElementsName @ r,
		"ElementNames" -> ElementNames @ r,
		"Names" -> Append[ Names[r], s ],
		"Barcode"             -> Barcode @ r,
		"FormalParameters"    -> FC @ r,
		"DirectProductDecompositions" -> WhichDecompositions @ r,
		"SubFusionRings"      -> SubFusionRings @ r  			
	];

AddName[ r_FusionRing?FusionRingQ, names_List ] := 
	FusionRing @ Sequence[
		"MultiplicationTable" -> MultiplicationTable @ r,
		"ElementsName" -> ElementsName @ r,
		"ElementNames" -> ElementNames @ r,
		"Names" -> Join[ Names[r], names ],
		"Barcode"             -> Barcode @ r,
		"FormalParameters"    -> FC @ r,
		"DirectProductDecompositions" -> WhichDecompositions @ r,
		"SubFusionRings"      -> SubFusionRings @ r  			
	];

SetNames[ r_FusionRing?FusionRingQ, names_List ] := 
	FusionRing @ Sequence[
		"MultiplicationTable" -> MultiplicationTable @ r,
		"ElementsName" -> ElementsName @ r,
		"ElementNames" -> ElementNames @ r,
		"Names" -> names,
		"Barcode"             -> Barcode @ r,
		"FormalParameters"    -> FC @ r,
		"DirectProductDecompositions" -> WhichDecompositions @ r,
		"SubFusionRings"      -> SubFusionRings @ r  			
	];


(* ::Subsubsection:: *)
(*Combining, decomposing and comparing fusion rings*)


SetAttributes[ EquivalentFusionRings, Listable ];
EquivalentFusionRings[ r_FusionRing?FusionRingQ ] := With[{ 
	l =  Permutations[ Range[Rank[r] - 1 ] ] + 1 },
	Table[ PermutedRing[ r, Join[ {1}, \[Sigma] ] ], { \[Sigma], l } ]
];

AllPermutations[ l1_List, l2_List ] :=
	Select[ Tuples[ PositionIndex[l1] /@ l2 ], Apply[Unequal] ];

PossiblePermutationVectors[ l_List ] := 
	AllPermutations[ l, l ];

 FusionRingAutomorphisms[ ring_FusionRing?FusionRingQ ] := With[{
	mt = MultiplicationTable @ ring,
	rk = Rank @ ring },
	possiblePerms = Prepend[1] /@ (
		PossiblePermutationVectors[ 
			Rest[ Count[ x_/; x > 0 ] /@ Diagonal[ mt ] ]
		] + 1
	);
	Cases[ possiblePerms, perm_/; PermuteMultTab[ mt, perm ] === mt ]
];

EquivalentMultiplicationTableQ[ tab1_, tab2_ ] := With[{
	d1 = Rest[ Count[ #, x_/; x > 0 ]& /@ Diagonal[ tab1 ] ],
	d2 = Rest[ Count[ #, x_/; x > 0 ]& /@ Diagonal[ tab2 ] ],
	n = Length[ tab1 ]}, 
	If[ Sort[d1] =!= Sort[d2],
		False,
		With[{
			prePerm1 = Prepend[#,1]& @ (PermutationList[ FindPermutation[ Sort[d1], d1 ], n - 1 ] + 1),
			prePerm2 = Prepend[#,1]& @ (PermutationList[ FindPermutation[ Sort[d1], d2 ], n - 1 ] + 1)},
			With[{
				newTab1 = PermuteMultTab[ tab1, prePerm1 ],
				newTab2 = PermuteMultTab[ tab2, prePerm2 ],
				possiblePerms = PossiblePermutationVectors[ Sort[ d1 ] ] },
				Module[{ 
					\[Sigma],
					equiv = False},
					Do[ 
						\[Sigma] = Prepend[ possiblePerms[[i]] + 1, 1 ];
						If[ PermuteMultTab[ newTab1, \[Sigma] ] === newTab2,
							equiv = True; Break[]
						]
					,{ i, Length[ possiblePerms ]}];
					Return[equiv]
				]
			]
		]
	]
];



EquivalentFusionRingQ[ r1_FusionRing?FusionRingQ, r2_FusionRing?FusionRingQ ] := With[{
	nFusionOutcomes1 = Map[ Count[ #, x_/; x > 0 ]&, MultiplicationTable[r1], {2}],
	nFusionOutcomes2 = Map[ Count[ #, x_/; x > 0 ]&, MultiplicationTable[r2], {2}]},
	With[{
		check = And[
			NNonZeroStructureConstants[r1] === NNonZeroStructureConstants[r2],
			NSelfDualNonSelfDual[r1] === NSelfDualNonSelfDual[r2],
			Sort[ Flatten[ nFusionOutcomes1 ] ] === Sort[ Flatten[ nFusionOutcomes2 ] ],
			Sort[ Diagonal[ nFusionOutcomes1 ] ] === Sort[ Diagonal[ nFusionOutcomes2 ] ],
			CommutativeQ[r1] == CommutativeQ[r2],
			Multiplicity[r1] == Multiplicity[r2]
	  ]},
		If[ !check,
		False,
		EquivalentMultiplicationTableQ[ MultiplicationTable[r1], MultiplicationTable[r2] ]
		]
	]
];

DirectProduct[ ring1_FusionRing?FusionRingQ, ring2_FusionRing?FusionRingQ ] := 
With[{ 
  k1 = Rank @ ring1, 
	k2 = Rank @ ring2,
	tab1 = MultiplicationTable[ring1],
	tab2 = MultiplicationTable[ring2]
	},
	FusionRing @ Rule[ "MultiplicationTable", 
			Flatten[ #, {{1,2},{3,4}} ]& @
				Table[ 
					Flatten @ Outer[ Times, tab1[[m1,n1]], tab2[[m2,n2]] ],
				{m1,k1},{m2,k2},{n1,k1},{n2,k2}
			] 
		]
];

DirectProduct[ ring1_FusionRing?FusionRingQ, ring2_FusionRing?FusionRingQ, rings__ ] := 
	DirectProduct[ DirectProduct[ ring1, ring2 ] , rings ];

(* We will use a function to determine all partitions of a set in order
   to calculate all multiplicative partitions of a number. The implementation 
   is taken directly from the source code of Combinatorica, found in the book 
   Computational Discrete Mathematics: Combinatorics and Graph Theory with Mathematica \[RegisteredTrademark]	Volume:
   Author(s):	Sriram Pemmaraju, Steven Skiena 
   
   First we define KSetPartitions which returns all partitions of a set
   into k subsets.
*)  
PrependAt[ listoflists_, element_, position_ ] := Block[{
	l = listoflists},
	PrependTo[ l[[position]], element ];
	l
];

KSetPartitions[ {} ][ 0 ] := { {} };
KSetPartitions[ s_List ][ 0 ] := {};
KSetPartitions[ s_List ][ k_Integer ] := {} /; ( k > Length[s] )
KSetPartitions[ s_List ][ k_Integer ] := { Map[ { # }&, s ] } /; (k === Length[s])
KSetPartitions[ s_List ][ k_Integer ] := Block[{
	$RecursionLimit = Infinity},
	Join[(* Put first element in subset of its own *)
		Prepend[ #, {First[s]} ] & /@ KSetPartitions[ Rest[s] ][ k-1 ],
		 (* Put first el in one of the subsets  *)
		Table[ PrependAt[ #, First @ s, j ], {j,Length[#]}]& /@ KSetPartitions[ Rest[s] ][ k ] // Flatten[#,1]&
	]		
]/; ( k > 0 ) && ( k < Length[s] )  

(* MultiplicativePartitions[int] returns list of integer tuples {i1,...,in} such 
   that i1*...*in \[Equal] int. This is needed to find all possible ranks of rings whose
   direct product could equal the given ring *)
   
SetPartitions[ {} ] := { {} };
SetPartitions[ s_List ] := Array[ KSetPartitions[ s ], Length[s] ] // Flatten[#,1]&

MultiplicativePartitions[x_] := Rest[
	DeleteDuplicates[ 
	Sort /@ 
		Map[ Times @@ # &,
			SetPartitions[
				Flatten[ ConstantArray @@@ FactorInteger[x] ]
			],
		{2}]
	]
];

	
internalBackTrackQDims[ qDims_, nRings_ , goal_, current_, ringNs_ , ringN_ ] := 
	If[ ringN > nRings && Chop[N[goal - current ],10^-5] == 0, 
		Sow[ ringNs ],
		If[ ringN <= nRings && Chop[N[goal - current],10^-5] > 0,
			Do[
				internalBackTrackQDims[ qDims, nRings, goal, current * qDims[[ringN,i]] , Append[ ringNs, i ], ringN + 1 ]
			,{ i, Length[ qDims[[ringN]] ] }];
		];
	];
	
BackTrackQDims[ quantumDims_, tqds_ ] := Block[{
	$RecursionLimit = Infinity},
	Reap[ internalBackTrackQDims[ quantumDims, Length[quantumDims], tqds, 1, {}, 1 ] ]// Last
];

CandidatesByTQDS[ r_FusionRing?FusionRingQ, list_ ] := Module[{
	tqds = TotalQuantumDimensionSquared[r],
	R = Rank[ r ],
	partitions,
	AllQDims,
	ranks,
	ringNumbers,
	rings},
	If[ PrimeQ[ R ],
		{},
		partitions = MultiplicativePartitions[ R ];
		rings = Table[ Cases[ list, ring_/; Rank[ring] == ran ] , { p, partitions }, { ran, p } ];
		AllQDims = Map[ TotalQuantumDimensionSquared, rings, {3} ];
		Table[{ rings[[i]], BackTrackQDims[ AllQDims[[i]], tqds ] }, {i, Length[partitions]} ] //
			Cases[ #, {rings_,{numbers_}} :> Table[ MapThread[ Part, { rings, ind } ], {ind, numbers} ] ]& //
			Flatten[ #, 1 ]&
	]
];

WhichDecompositions[ r_FusionRing?FusionRingQ, list_List ] := 
	If[
		r["DirectProductDecompositions"] =!= Missing[],
		ReleaseHold @ r["DirectProductDecompositions"],
		With[{
			candidates = CandidatesByTQDS[ r, list ] },
			DeleteDuplicates[
				Sort /@ Cases[ candidates, rings_/; EquivalentFusionRingQ[ r, DirectProduct @@ rings ] ]
			]
		]
	];

WhichDecompositions[ r_FusionRing?FusionRingQ ] :=
	WhichDecompositions[ r, FusionRingList ];

WD[ r__] := WhichDecompositions[ r ];


(*Finding subrings*)
(*Check whether the multiplication is internal*)
InternalMultiplicationQ[ multTab_, particles_ ] := With[{
	comp = Complement[ Range[ Length[multTab] ], particles ]},
	MatchQ[ Flatten @ multTab[[ particles, particles, comp ]], {Repeated[0]} ]
];

SubsetChoices[ multTab_ ] := With[{
	apPairs = Sort /@ ( Position[ multTab[[2;;,2;;,1]], 1 ] + 1)/.{a_,a_}->{a} },
	Prepend[ #, 1 ]& /@
	Join @@@
	Subsets[ DeleteDuplicates[ apPairs ] ][[2;;-2]] 
];

SubRingTables[ multTab_ ] := With[{
	IMQ = InternalMultiplicationQ[ multTab, # ]&,
	subsets = SubsetChoices[ multTab ] },
	<| "Subset"-> #, "MultTab"-> multTab[[#,#,#]] |>& /@
		Select[ subsets, IMQ ]
];

RingsFromParams[ nsdnsd_List, mult_Integer, nnzsc_Integer ] :=
RingsFromParams[ nsdnsd, mult, nnzsc ] = 
	FRL //
	Cases[ ring_/; 
		NNZSC[ring] == nnzsc &&
		NSDNSD[ring] == nsdnsd && 
		Mult[ring] == mult
	];

(*Replace a fusion ring by the first known equivalent ring in the FRL*)
SetAttributes[ ReplaceByKnown, Listable ];
ReplaceByKnown[ ring_ ] := Module[{
	equivRing},
	With[{
		knownRings = RingsFromParams[ NSDNSD[ring], Mult[ring], NNZSC[ring] ]},
		equivRing = FirstCase[ knownRings, r_/;EquivalentFusionRingQ[ r, ring ] ];
		If[ Head[equivRing] =!= Missing,
			equivRing,
			ring
		]
	]
];

SetAttributes[ SubFusionRings, Listable ];
SubFusionRings[ ring_FusionRing?FusionRingQ] :=
	If[ 
		ring["SubFusionRings"] =!= Missing[],
		ReleaseHold @ ring["SubFusionRings"],
		If[ Rank[ring] === 1,
			{},
			Module[ { 
				m = MT[ring],
				ToRing = FusionRing[ "MultiplicationTable" -> # ]&,
				multTabs},
				multTabs = SubRingTables[m];
				<| 
					"SubSet" -> #["Subset"], 
					"SubFusionRing"-> ReplaceByKnown[ ToRing[ #["MultTab"] ] ]
				|>& /@ multTabs
			]
		]
	];
	
InjectionForm[ ring_FusionRing?FusionRingQ, subring_FusionRing?FusionRingQ ] := Module[{
	r = Rank[ring],
	mt = MT[ring],
	rs = Rank[subring],
	mts = MT[subring],
	ds,
	equivTable,
	permutation,
	possiblePerms
	},
	equivTable = SelectFirst[ SubRingTables[ mt ], EquivalentMultiplicationTableQ[ mts, #["MultTab"] ]& ];
	possiblePerms = With[ { diagonalChannels = Rest[ Count[ x_/; x > 0 ] /@ Diagonal[ # ] ]&},
		Prepend[1] /@ (
		AllPermutations[
			diagonalChannels[ mts ],
			diagonalChannels[ equivTable["MultTab"] ]
		] + 1 )
	];
	permutation = FirstCase[ possiblePerms, s_/; mts[[s,s,s]] == equivTable["MultTab"] ];
	Association @@ 
	MapThread[ 
		Rule, 
		{ Range[ rs ], equivTable["Subset"][[permutation]] }
	]
]


(* ::Subsubsection::Closed:: *)
(*Working with elements*)


(* Define the fusion product between two elements *)
FusionProduct[ r_FusionRing?FusionRingQ, { el1_, el2_ } ] := 
	With[ { tab = MultiplicationTable[r] },
		Sum[ r[[i]]tab[[ el1, el2, i ]], { i, Rank[r] } ]
	];
	

FusionElement /: QuantumDimension[ FusionElement[ r_FusionRing?FusionRingQ, el_ ] ] := 
QuantumDimension[ FusionElement[ r, el ] ] :=
	(QuantumDimensions[ r ])[[ el ]]

FusionElement /: QD[  el_FusionElement  ] :=
	QuantumDimension[ el ];


(* ::Subsubsection:: *)
(*Dataset*)


importDirectory = Quiet[ 
	Check[ SetDirectory @ DirectoryName @ $InputFileName,    (* If not using notebook interface *)
	SetDirectory @ NotebookDirectory[]],SetDirectory::fstr   (* If using notebook interface *)
];

optimizedImport[ fileName_String, importDirectory_String ] := Module[{
	data},
	If[(* Have MX file *)
		MemberQ[ FileNames[], fileName <> ".mx" ],
		(* THEN: Import  MX file *)
		Import[ FileNameJoin[{ importDirectory, fileName <> ".mx" }], "MX" ],
		(* ELSE: Create MX file from WDX file *)
		PrintTemporary["Import not yet optimized for this machine. Optimizing for future use..."];
		data = Uncompress @ Import[ FileNameJoin[{ importDirectory, fileName<>".wdx" }], "WDX" ];
		Export[ FileNameJoin[{ importDirectory, fileName <> ".mx" }], data, "MX" ];
		PrintTemporary["Import optimized for future use. Importing data..."];
		data
	]
];

PrintTemporary["Importing FusionRingList"];
FRL = FusionRingList = optimizedImport[ "FusionRingList", importDirectory ];

AllFusionRingsQ[ r_Integer, m_Integer ] := 
	Or[
		r == 1, 
		MemberQ[r] @ { 2, 3, 4 } && m <= 16,
		r == 5 && m <= 12, 
		r == 6 && m <= 4,
		r == 7 && m <= 2, 
		r == 8 && m == 1,
		r == 9 && m == 1
	];
	
AFRQ[ r_, m_ ] := 
	AllFusionRingsQ[ r, m ];
	
FusionRingByCode = Association @@ MapThread[ Rule, { FormalCode /@ FRL, FRL } ];
FRBC = FusionRingByCode;


(* ::Subsubsection:: *)
(*Exporting data*)


(* For the current wiki layout the table should have its first row equal to
{"Name", "Rank", "Number non-selfdual", "<math> \\mathcal{D}^{2}_{PF} </math>", "Number nonzero structure constants", "Largest structure constant", "Can be fusion cat", "Can be braided", "Can be modular", "Can be unitary" } 
*)
tableHeader[ firstRow_List ] := 
	"{| class=\"wikitable sortable\"\n" <>
	StringJoin @@ Table["!" <> name <> "\n", { name, firstRow } ] <>
	"|-\n";

tableRow[ row_ ] := 
	StringJoin @@ Table[ "|| " <> ToString[ el ]<>" ", { el, row } ] <> "\n|-\n";

tableEnd[] := 
	"|}";
	
ToWikiTable[ table_ ] := 
	tableHeader[ table // First ] <>
	StringJoin @@ Table[ tableRow[row], { row, table[[2;;]] } ] <>
	tableEnd[];


(* ::Subsection:: *)
(*Formatting and notation *)


(* ::Subsubsection:: *)
(* Formatting *)


Format[ ring:FusionRing[r_Association], StandardForm ] := 
	If[ r["Names"] === {},
		If[
			r["FormalParameters"] =!= Missing[],
			FusionRing[ Sequence @@ r["FormalParameters"] ],
			FusionRing[ Rank[ring], Multiplicity[ring], NNSD[ring], "_" ]
		],
		FusionRing[ r["Names"] // First ]
	]

Format[ FusionElement[ a_, el_Integer], StandardForm ] := 
	Format[ FusionElement[ a, el], StandardForm ] =
		If[ 1 <= el <= Rank[a], 
			a["ElementNames"][[el]],
			Message[ FusionRing::eloutofbounds ]
		];

Format[ FusionElement[ a_, el_String], StandardForm ] :=
	Format[ FusionElement[ a, el], StandardForm ] =  
		If[ MemberQ[a["ElementNames"], el ], 
			el, 
			Message[ FusionRing::elnotfound, el ] 
		];
		
Format[ FusionElement[ a_, el_Symbol], StandardForm ] :=
	Format[ FusionElement[ a, el], StandardForm ] = 
		If[ MemberQ[a["ElementNames"], ToString[el] ], 
			ToString[el], 
			Message[ FusionRing::elnotfound, el ] 
		];


(* ::Subsubsection::Closed:: *)
(* Notation *)


(* Call particles by invoking ring[[el]], where el is either Integer,
   or a Symbol or String corresponding to one of the strings defined
   in ring[ElementNames] *)
   
FusionRing /: r_FusionRing?FusionRingQ[[el_Integer]] := 
	If[ 0 < el <= Rank[r], 
		FusionElement[ r, el ],
		Message[ FusionRing::eloutofbounds, el ]
	];

FusionRing /: r_FusionRing?FusionRingQ[[el_String]] := 
With[{
	pos = Position[ElementNames[r], el ]},
	If[ Length[pos] == 0,
		Message[ FusionRing::elnotfound, el ],
		FusionElement[ r, pos[[1,1]] ]
	]
];

FusionRing /: r_FusionRing?FusionRingQ[[el_Symbol]] := 
With[{
	pos = Position[ElementNames[r], ToString[el] ]},
	If[ Length[pos] == 0,
		Message[ FusionRing::elnotfound, el ],
		FusionElement[ r, pos[[1,1]] ]
	]
];

(* Use CenterDot (esc . esc) notation for the fusion product between elements *)
FusionElement /: 
	CenterDot[ 
		FusionElement[ r_FusionRing?FusionRingQ, el1_ ], 
		FusionElement[ r_FusionRing?FusionRingQ, el2_ ] ] := FusionProduct[ r, { el1, el2 } ];

(* Encode linearity of the multiplication *)

CenterDot[ a___, b_?IntegerQ * x_, c___ ] :=  b * CenterDot[ a, x, c ];
CenterDot[ a___, ( x_ + y_ ), c___ ] := Expand[ CenterDot[ a, x, c ] + CenterDot[ a, y, c ] ];
CenterDot[ x_ ] := x ;
CenterDot[ a_Integer, b_Integer ] := a * b;
CenterDot[ a_, b_, c__] := Expand[ CenterDot[ CenterDot[ a, b ], CenterDot[c] ] ];


(* ::Section::Closed:: *)
(*End `Private` Context*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


EndPackage[]
