(* Workaround for stuff *)
ParallelEvaluate[
 BoxForm`$UseTemplateSlotSequenceForRow = False;]
(* Don't think this actually does anything, but I don't dare to touch \
anything *)
ParallelEvaluate[$NumberMarks = False]

constructDatapoint[type_, name_, propertyElement_] := 
 Module[{property = propertyElement, 
   valueProcessor = Function[{x, y}, y], val, result = {}},
  Switch[Head[propertyElement],
   List, property = propertyElement[[1]]; 
   valueProcessor = 
    propertyElement[[2]];(*If the element contains a preprocessor, 
   use that*)
   ];
  val = valueProcessor[name, type[name, property]];
  AppendTo[result, "data" -> val];
  If[Head[type[name, property, "Description"]] =!= Missing &&
    
    type[name, property, "Description"] =!= "" &&
    
    Head[type[name, property, "Description"]] =!= type,
   AppendTo[result, 
    "tex_description" -> 
     ToString[type[name, property, "Description"], TeXForm]]];
  If[Head[type[name, property, "Note"]] =!= Missing &&
    
    type[name, property, "Note"] =!= "" &&
    
    Head[type[name, property, "Note"]] =!= type,
   AppendTo[result, 
    "tex_note" -> ToString[type[name, property, "Note"], TeXForm]]];
  property -> result
  ]

(* This could propably be constructed dynamically (with overrides), \
but there are some things I don't want exported, so I can't be \
bothered... *)

elementProperties := {"Name", "StandardName", "Symbol", 
   "AtomicNumber", "CASNumber", "Abbreviation", "AtomicMass", 
   "Density", "LiquidDensity", "MeltingPoint", 
   "BoilingPoint", {"Phase", 
    Function[{i, d}, d /. e_Entity :> ToString[CanonicalName[e]]]}, 
   "CriticalPressure", "CriticalTemperature", "FusionHeat", 
   "VaporizationHeat", "SpecificHeat", "AdiabaticIndex", "NeelPoint", 
   "ThermalConductivity", "ThermalExpansion", "MolarVolume", 
   "BrinellHardness", "MohsHardness", "VickersHardness", 
   "BulkModulus", "ShearModulus", "YoungModulus", "PoissonRatio", 
   "RefractiveIndex", "SoundSpeed", "Valence", "Electronegativity", 
   "ElectronAffinity", "IonizationEnergies", "AlternateNames", 
   "AllotropeNames", "Block", "Group", "Period", "Series", 
   "ElectronConfiguration", 
   "ElectronShellConfiguration", {"ElectronConfigurationString", 
    Function[{i, d}, 
     ToString[d /. {Superscript -> Power}, TeXForm]]}, "Color", 
   "DiscoveryCountries", "DiscoveryYear", "ElectricalType", 
   "ElectricalConductivity", "Resistivity", "SuperconductingPoint", 
   "MagneticType", "CuriePoint", "MassMagneticSusceptibility", 
   "MolarMagneticSusceptibility", {"VolumeMagneticSusceptibility", 
    Function[{i, d}, ToString[d, CForm]]}, "UniverseAbundance", 
   "SolarAbundance", "MeteoriteAbundance", "CrustAbundance", 
   "OceanAbundance", "HumanAbundance", "AtomicRadius", 
   "CovalentRadius", "VanDerWaalsRadius", "CrystalStructure", 
   "LatticeAngles", 
   "LatticeConstants", {"SpaceGroupName", 
    Function[{i, d}, ToString[d, TeXForm]]}, "SpaceGroupNumber", 
   "HalfLife", "Lifetime", 
   "DecayMode", {"QuantumNumbers", 
    Function[{i, d}, 
     ToString[d /. {Superscript -> Power}, TeXForm]]}, 
   "NeutronCrossSection", "NeutronMassAbsorption", "KnownIsotopes", 
   "StableIsotopes", {"IsotopeAbundances", 
    Function[{i, d}, 
     Module[{isotopes = {}}, 
      Do[AppendTo[isotopes, ToString[v[[1]], CForm] -> v[[2]]], {v, 
        d}]; isotopes]]}, "KnownOxidationStates"};
isotopeProperties := {"AtomicMass", "AtomicNumber", "BindingEnergy", 
  "BranchingRatios", "DaughterNuclides", "DecayEnergies", 
  "DecayModes", "DecayProducts", "ExcitedStateEnergies", 
  "ExcitedStateHalfLives", "ExcitedStateLifetimes", 
  "ExcitedStateParities", "ExcitedStateSpins", "ExcitedStateWidths", 
  "HalfLife", "IsotopeAbundance", "Lifetime", "MagneticMoment", 
  "MassExcess", "MassNumber", "Memberships", "Name", "NeutronNumber", 
  "Parity", "QuadrupoleMoment", 
  "QuantumStatistics", {"Spin", 
   Function[{i, d}, ToString[d, TeXForm]]}, "Stable", 
  "StandardName", {"Symbol", 
   Function[{i, d}, ToString[d /. Superscript -> Power, TeXForm]]}, 
  "Width"}

constructIsotopeData[n_] := 
 Module[{properties = {}}, 
  Do[AppendTo[properties, constructDatapoint[IsotopeData, n, p]], {p, 
    isotopeProperties}]; properties]
constructIsotopesData[i_] := 
 Map[Function[isotope, 
   ToString[isotope] -> 
    constructIsotopeData[
     ElementData[i, "StandardName"] <> ToString[isotope]]], 
  ElementData[i, "KnownIsotopes"]]

(* Function takes a element number, gives a map with element data *)

constructElementData[i_] := Module[{dataPoints = {}},
  Do[AppendTo[dataPoints, constructDatapoint[ElementData, i, v]], {v, 
    elementProperties}]; (* 
  Add all the properties in the dataTypes list *)
  
  AppendTo[dataPoints, "IsotopeData" -> constructIsotopesData[i]];(* 
  Construct and add isotope data *)
  dataPoints
  ]

(* Replace json-incompatible types with json-compatible ones *)

jsonPreProcess[d_] := d /. {
   s_Quantity :> {
     "value" -> ToString[QuantityMagnitude[s], CForm],
     "tex_unit" -> 
      ToString[QuantityForm[QuantityUnit[s], "Abbreviation"], 
       TeXForm]
     }, 
   Missing[s_, ___] :> s,
   s_Entity :> CommonName@s,
   s_EntityClass :> CommonName@s,
   s_DateObject :> DateString@s,
   _DirectedInfinity -> "Infinity"}

elementsData := 
 Parallelize[
  Map[Function[element, 
    ToString[element["AtomicNumber"]] -> 
     jsonPreProcess[constructElementData[element["AtomicNumber"]]]], 
   ElementData[]]]

elementsData

(* Debug code. Prints all different types(heads) in the elements map *)
\
(*heads:={}
uniqueAppend:=Function[{element},If[MemberQ[heads,element],False,\
AppendTo[heads,element];True]]
Scan[Function[x, If[uniqueAppend[Head[x]],Print[x]]],elementsData, \
Infinity];
heads*)

(* Export data to JSON *)
Export["elements.json", elementsData, \
"JSON"]
