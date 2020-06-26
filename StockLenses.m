(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["StockLenses`"];


getLens::usage = "return a lens object for a specified vendor's catalogue identificator";
readZemaxSingletSphericalLensFile::usage = "reads in a spherical lens specifications witten in a zmx file. The user needs to specify the four letter code (e.g. DIAM or MEMA) that specifies the line that contains the physical radius (contrary to intuition, DIAM is for radius) Use the CharacterEncoding to specify the encoding, usually ASCII or Unicode";
readThorlabsZemaxSingletAsphericalLensFile::usage = "reads in a aspherical lens specifications witten in a zmx file. The user needs to specify the four letter code (e.g. DIAM or MEMA) that specifies the line that contains the physical radius (contrary to intuition, DIAM is for radius) Use the CharacterEncoding to specify the encoding, usually ASCII or Unicode";


Begin["`Private`"];


getLens[catalogueName_String]:=With[{listOfLensAssociationsToSerch=Flatten[Values[allLenses],1]},getLens[catalogueName,listOfLensAssociationsToSerch]]


getLens[catalogueName_String, manufacturer_String]:=With[{manufacturerStandardName=getManufacturerNameStandard[manufacturer]},If[MissingQ[manufacturerStandardName],Print["Unknown manufacturer "<>manufacturer], With[{manufacturerLenses=allLenses[manufacturerStandardName]}, getLens[catalogueName,manufacturerLenses]]]]


getLens[catalogueName_String,listOfLensAssociationsToSerch_List] := ReleaseHold[Module[{isFound=False,lens},With[{catalogueNameSimplified=StringReplace[ToUpperCase[catalogueName],{"#"->"","-"->""}]},Do[If[KeyExistsQ[listOfLensAssociationsToSerch[[i]],catalogueNameSimplified],isFound=True;lens=listOfLensAssociationsToSerch[[i]][catalogueNameSimplified];Return[]],{i,1,Length[listOfLensAssociationsToSerch]}];If[isFound,lens,Print["Lens not found"];Null]]]]


thorlabsManufacturerName = "THORLABS";
edmundOpticsManufacturerName = "EDMUND_OPTICS";


manufacturerStandardNames=<|"EDMUND_OPTICS"->edmundOpticsManufacturerName,"EDMUNDOPTICS"->edmundOpticsManufacturerName,"EDMUND"->edmundOpticsManufacturerName,"EDMUND OPTICS"->edmundOpticsManufacturerName,
"THORLABS"->thorlabsManufacturerName,"THOR LABS"->thorlabsManufacturerName|>;


getManufacturerNameStandard[synonym_String]:=With[{synonymUpperCase=ToUpperCase[synonym]},With[{standardName=manufacturerStandardNames[synonymUpperCase]},standardName]];


(* ::Input::Initialization:: *)
Options[readZemaxSingletSphericalLensFile]={CharacterEncoding->"ASCII"};


(* ::Input::Initialization:: *)
readZemaxSingletSphericalLensFile[file_,semiDiameterLine_,opts:OptionsPattern[readZemaxSingletSphericalLensFile]]:=With[{lensNameLine="NAME",(*the id of the lens is in the comment of the first surface*)commentLine="COMM",curvatureNameLine="CURV",glassNameLine="GLAS",glassCatalogueLine="GCAT",centerThicknessLine="DISZ",surface1Line="SURF 1",surface2Line="SURF 2",curvatureLine="CURV"},With[{rawData=Import[file,"Table",opts]},With[{indices = Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]==="SURF"&)]]},With[{header=Take[rawData,{1,indices[[1]]}],curvatureIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===curvatureNameLine&)]],glassIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===glassNameLine&)]],semiDiameterIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===semiDiameterLine&)]],centerThicknessDiameterIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===centerThicknessLine&)]],commentIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===commentLine&)]],firstSurfaceIndex=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==1,Return[indices[[i]]]],{i,1,Length[indices]}],secondSurfaceIndex=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==2,Return[indices[[i]]]],{i,1,Length[indices]}],thirdSurfaceIndex=With[{third=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==3,Return[indices[[i]]]],{i,1,Length[indices]}]},If[third===Null,Length[rawData],third]]},With[{nameIndex=With[{allIndices=Flatten[Position[header,_?(Length[#]>= 1&&#[[1]]===lensNameLine&)]]},If[Length[allIndices]>= 1,allIndices[[1]],-1]],glassCatalogueIndex=With[{allIndices=Flatten[Position[header,_?(Length[#]>= 1&&#[[1]]===glassCatalogueLine&)]]},If[Length[allIndices]>= 1,allIndices[[1]],-1]],commentIndex=With[{selected=Select[commentIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)]},If[Length[selected]>= 1,selected[[1]],-1]],firstCurvatureIndex=Select[curvatureIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)][[1]],glassIndex=Select[glassIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)][[1]],semiDiameterIndex=Select[semiDiameterIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)][[1]],centerThicknessIndex=Select[centerThicknessDiameterIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)][[1]],secondCurvatureIndex=Select[curvatureIndices,(#>secondSurfaceIndex&&#<thirdSurfaceIndex&)][[1]]},With[{idIndex=If[commentIndex>0,commentIndex,nameIndex],OpenRayTracer`GlassCatalogueData=If[glassCatalogueIndex>0,Take[rawData[[glassCatalogueIndex]],{2,-1}],{}]},With[{R1=If[Abs[rawData[[firstCurvatureIndex,2]]]>10^-12,N[Rationalize[1/rawData[[firstCurvatureIndex,2]],0.0001]],\[Infinity]],R2=If[Abs[rawData[[secondCurvatureIndex,2]]]>10^-12,N[Rationalize[1/rawData[[secondCurvatureIndex,2]],0.0001]],\[Infinity]],centerThickness=rawData[[centerThicknessIndex,2]],diameter=2*rawData[[semiDiameterIndex,2]],glass=rawData[[glassIndex,2]],lensName=StringReplace[ToUpperCase[ToString[rawData[[idIndex,2]]]],{"#"->"","-"->""}]},lensName->HoldForm[OpenRayTracer`createSphericalLens[0,(*radius of the first surface*)R1,(*radius of the second surface*)R2,centerThickness, (*physical diameter*)diameter,glass,OpenRayTracer`PartId->lensName,OpenRayTracer`GlassCatalogueData->OpenRayTracer`GlassCatalogueData]]]]]]]]]


(* ::Input::Initialization:: *)
readThorlabsZemaxSingletSphericalLensFile[file_,opts:OptionsPattern[readZemaxSingletSphericalLensFile]]:=readZemaxSingletSphericalLensFile[file,"DIAM",opts]


(* ::Input::Initialization:: *)
readEdmundOpticsZemaxSingletSphericalLensFile[file_,opts:OptionsPattern[readZemaxSingletSphericalLensFile]]:=readZemaxSingletSphericalLensFile[file,"MEMA",opts]


(* ::Input::Initialization:: *)
Options[readZemaxCementedDoubletSphericalLensFile]={CharacterEncoding->"ASCII"};


(* ::Input::Initialization:: *)
readZemaxCementedDoubletSphericalLensFile[file_,semiDiameterLine_,opts:OptionsPattern[readZemaxCementedDoubletSphericalLensFile]]:=With[{lensNameLine="NAME",(*the id of the lens is in the comment of the first surface*)commentLine="COMM",curvatureNameLine="CURV",glassNameLine="GLAS",glassCatalogueLine="GCAT",centerThicknessLine="DISZ",surface1Line="SURF 1",surface2Line="SURF 2",curvatureLine="CURV"},With[{rawData=Import[file,"Table",opts]},With[{indices = Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]==="SURF"&)]]},With[{header=Take[rawData,{1,indices[[1]]}],curvatureIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===curvatureNameLine&)]],
glassIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===glassNameLine&)]],
semiDiameterIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===semiDiameterLine&)]],
centerThicknessDiameterIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===centerThicknessLine&)]],
commentIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===commentLine&)]],
firstSurfaceIndex=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==1,Return[indices[[i]]]],{i,1,Length[indices]}],secondSurfaceIndex=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==2,Return[indices[[i]]]],{i,1,Length[indices]}],thirdSurfaceIndex=With[{third=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==3,Return[indices[[i]]]],{i,1,Length[indices]}]},If[third===Null,Length[rawData],third]],fourthSurfaceIndex=With[{fourth=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==4,Return[indices[[i]]]],{i,1,Length[indices]}]},If[fourth===Null,Length[rawData],fourth]]},With[{nameIndex=With[{allIndices=Flatten[Position[header,_?(Length[#]>= 1&&#[[1]]===lensNameLine&)]]},If[Length[allIndices]>= 1,allIndices[[1]],-1]],glassCatalogueIndex=With[{allIndices=Flatten[Position[header,_?(Length[#]>= 1&&#[[1]]===glassCatalogueLine&)]]},If[Length[allIndices]>= 1,allIndices[[1]],-1]],commentIndex=With[{selected=Select[commentIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)]},If[Length[selected]>= 1,selected[[1]],-1]],firstCurvatureIndex=Select[curvatureIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)][[1]],
secondCurvatureIndex=Select[curvatureIndices,(#>secondSurfaceIndex&&#<thirdSurfaceIndex&)][[1]],
thirdCurvatureIndex=Select[curvatureIndices,(#>thirdSurfaceIndex&&#<fourthSurfaceIndex&)][[1]],
firstGlassIndex=Select[glassIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)][[1]],
secondGlassIndex=Select[glassIndices,(#>secondSurfaceIndex&&#<thirdSurfaceIndex&)][[1]],
semiDiameterIndex=Select[semiDiameterIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)][[1]],
firstCenterThicknessIndex=Select[centerThicknessDiameterIndices,(#>firstSurfaceIndex&&#<secondSurfaceIndex&)][[1]],
secondCenterThicknessIndex=Select[centerThicknessDiameterIndices,(#>secondSurfaceIndex&&#<thirdSurfaceIndex&)][[1]]},With[{idIndex=If[commentIndex>0,commentIndex,nameIndex],OpenRayTracer`GlassCatalogueData=If[glassCatalogueIndex>0,Take[rawData[[glassCatalogueIndex]],{2,-1}],{}]},With[{R1=If[Abs[rawData[[firstCurvatureIndex,2]]]>10^-12,N[Rationalize[1/rawData[[firstCurvatureIndex,2]],0.0001]],\[Infinity]],R2=If[Abs[rawData[[secondCurvatureIndex,2]]]>10^-12,N[Rationalize[1/rawData[[secondCurvatureIndex,2]],0.0001]],\[Infinity]],
R3=If[Abs[rawData[[thirdCurvatureIndex,2]]]>10^-12,N[Rationalize[1/rawData[[thirdCurvatureIndex,2]],0.0001]],\[Infinity]],
firstCenterThickness=rawData[[firstCenterThicknessIndex,2]],
secondCenterThickness=rawData[[secondCenterThicknessIndex,2]],
diameter=2*rawData[[semiDiameterIndex,2]],
firstGlass=rawData[[firstGlassIndex,2]],
secondGlass=rawData[[secondGlassIndex,2]],lensName=StringReplace[ToUpperCase[ToString[rawData[[idIndex,2]]]],{"#"->"","-"->""}]},lensName->HoldForm[OpenRayTracer`createCementedSphericalDoubletLens[0,R1,R2,R3,firstCenterThickness,secondCenterThickness, diameter,firstGlass,secondGlass,OpenRayTracer`PartId->lensName,OpenRayTracer`GlassCatalogueData->OpenRayTracer`GlassCatalogueData]]]]]]]]]


(* ::Input::Initialization:: *)
readThorlabsZemaxCementedDoubletSphericalLensFile[file_,opts:OptionsPattern[readZemaxCementedDoubletSphericalLensFile]]:=readZemaxCementedDoubletSphericalLensFile[file,"DIAM",opts]


(* ::Input::Initialization:: *)
readEdmundOpticsZemaxCementedDoubletSphericalLensFile[file_,opts:OptionsPattern[readZemaxCementedDoubletSphericalLensFile]]:=readZemaxCementedDoubletSphericalLensFile[file,"MEMA",opts]


(* ::Input::Initialization:: *)
Options[readZemaxSingletAsphericalLensFile]={CharacterEncoding->"ASCII",TreatSurface1lAwaysAsFirstFaceOfLens->False};


(* ::Input::Initialization:: *)
readZemaxSingletAsphericalLensFile[file_,semiDiameterLine_,opts:OptionsPattern[readZemaxSingletAsphericalLensFile]]:=With[{lensNameLine="NAME",(*the id of the lens is in the comment of the first surface*)commentLine="COMM",curvatureNameLine="CURV",glassNameLine="GLAS",glassCatalogueLine="GCAT",conicalLine="CONI",asphericParameterLine="PARM",centerThicknessLine="DISZ",typeLine="TYPE",typeEvenAsph="EVENASPH",typeStandard="STANDARD",curvatureLine="CURV"},With[{rawData=Import[file,"Table",opts]},With[{indices = Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]==="SURF"&)]]},With[{header=Take[rawData,{1,indices[[1]]}],commentIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===commentLine&)]],typeIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===typeLine&)]],curvatureIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===curvatureNameLine&)]],glassIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===glassNameLine&)]],semiDiameterIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===semiDiameterLine&)]],centerThicknessDiameterIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===centerThicknessLine&)]],conicalIndices=Flatten[Position[rawData,_?(Length[#]>= 1&&#[[1]]===conicalLine&)]],asphericParametersOrder2=Flatten[Position[rawData,_?(Length[#]>= 3&&#[[1]]===asphericParameterLine&&#[[2]]===1&)]],asphericParametersOrder4=Flatten[Position[rawData,_?(Length[#]>= 3&&#[[1]]===asphericParameterLine&&#[[2]]===2&)]],asphericParametersOrder6=Flatten[Position[rawData,_?(Length[#]>= 3&&#[[1]]===asphericParameterLine&&#[[2]]===3&)]],asphericParametersOrder8=Flatten[Position[rawData,_?(Length[#]>= 3&&#[[1]]===asphericParameterLine&&#[[2]]===4&)]],asphericParametersOrder10=Flatten[Position[rawData,_?(Length[#]>= 3&&#[[1]]===asphericParameterLine&&#[[2]]===5&)]],asphericParametersOrder12=Flatten[Position[rawData,_?(Length[#]>= 3&&#[[1]]===asphericParameterLine&&#[[2]]===6&)]],asphericParametersOrder14=Flatten[Position[rawData,_?(Length[#]>= 3&&#[[1]]===asphericParameterLine&&#[[2]]===7&)]],asphericParametersOrder16=Flatten[Position[rawData,_?(Length[#]>= 3&&#[[1]]===asphericParameterLine&&#[[2]]===8&)]],firstSurfaceIndexInit=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==1,Return[indices[[i]]]],{i,1,Length[indices]}],secondSurfaceIndexInit=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==2,Return[indices[[i]]]],{i,1,Length[indices]}],thirdSurfaceIndexInit=With[{third=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==3,Return[indices[[i]]]],{i,1,Length[indices]}]},If[third===Null,Length[rawData],third]],fourthSurfaceIndexInit=With[{fourth=Do[If[Length[rawData[[indices[[i]]]]]>= 2&&rawData[[indices[[i]],2]]==4,Return[indices[[i]]]],{i,1,Length[indices]}]},If[fourth===Null,Length[rawData],fourth]]},With[{firstSurfaceTypeIndex=With[{selected=Select[typeIndices,(#>firstSurfaceIndexInit&&#<secondSurfaceIndexInit&)]},If[Length[selected]>= 1,selected[[1]],-1]],secondSurfaceTypeIndex=With[{selected=Select[typeIndices,(#>secondSurfaceIndexInit&&#<thirdSurfaceIndexInit&)]},If[Length[selected]>= 1,selected[[1]],-1]],thirdSurfaceTypeIndex=With[{selected=Select[typeIndices,(#>thirdSurfaceIndexInit&&#<fourthSurfaceIndexInit&)]},If[Length[selected]>= 1,selected[[1]],-1]]},With[{firstLensFaceIndex=If[OptionValue[TreatSurface1lAwaysAsFirstFaceOfLens]||rawData[[firstSurfaceTypeIndex,2]]===typeStandard||rawData[[firstSurfaceTypeIndex,2]]===typeEvenAsph,firstSurfaceIndexInit,secondSurfaceIndexInit],secondLensFaceIndex=If[OptionValue[TreatSurface1lAwaysAsFirstFaceOfLens]||rawData[[firstSurfaceTypeIndex,2]]===typeStandard||rawData[[firstSurfaceTypeIndex,2]]===typeEvenAsph,secondSurfaceIndexInit,thirdSurfaceIndexInit],surfaceBeyondTheLens=If[OptionValue[TreatSurface1lAwaysAsFirstFaceOfLens]||rawData[[firstSurfaceTypeIndex,2]]===typeStandard||rawData[[firstSurfaceTypeIndex,2]]===typeEvenAsph,thirdSurfaceIndexInit,fourthSurfaceIndexInit]},With[{nameIndex=With[{selected=Flatten[Position[header,_?(Length[#]>= 1&&#[[1]]===lensNameLine&)]]},If[Length[selected]>= 1,selected[[1]],-1]],glassCatalogueIndex=With[{allIndices=Flatten[Position[header,_?(Length[#]>= 1&&#[[1]]===glassCatalogueLine&)]]},If[Length[allIndices]>= 1,allIndices[[1]],-1]],commentIndex=With[{selected=Select[commentIndices,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)]},If[Length[selected]>= 1,selected[[1]],-1]],firstCurvatureIndex=Select[curvatureIndices,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)][[1]],firstConicalIndex=With[{selected=Select[conicalIndices,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)]},If[Length[selected]>= 1,selected[[1]],-1]],secondCurvatureIndex=Select[curvatureIndices,(#>secondLensFaceIndex&&#<surfaceBeyondTheLens&)][[1]],secondConicalIndex=With[{selected=Select[conicalIndices,(#>secondLensFaceIndex&&#<surfaceBeyondTheLens&)]},If[Length[selected]>= 1,selected[[1]],-1]],glassIndex=Select[glassIndices,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)][[1]],semiDiameterIndex=Select[semiDiameterIndices,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)][[1]],centerThicknessIndex=Select[centerThicknessDiameterIndices,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)][[1]],


			firstSurfaceAsphericCoefficients={With[{selected=Select[asphericParametersOrder4,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder6,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder8,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder10,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder12,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder14,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder16,(#>firstLensFaceIndex&&#<secondLensFaceIndex&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]]},

		secondSurfaceAsphericCoefficients={With[{selected=Select[asphericParametersOrder4,(#>secondLensFaceIndex&&#<surfaceBeyondTheLens&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder6,(#>secondLensFaceIndex&&#<surfaceBeyondTheLens&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder8,(#>secondLensFaceIndex&&#<surfaceBeyondTheLens&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder10,(#>secondLensFaceIndex&&#<surfaceBeyondTheLens&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder12,(#>secondLensFaceIndex&&#<surfaceBeyondTheLens&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder14,(#>secondLensFaceIndex&&#<surfaceBeyondTheLens&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]],With[{selected=Select[asphericParametersOrder16,(#>secondLensFaceIndex&&#<surfaceBeyondTheLens&)]},If[Length[selected]>= 1,rawData[[selected[[1]],3]],0]]}},


With[{idIndex=If[commentIndex>0,commentIndex,nameIndex],OpenRayTracer`GlassCatalogueData=If[glassCatalogueIndex>0,Take[rawData[[glassCatalogueIndex]],{2,-1}],{}]},With[{name=StringReplace[ToUpperCase[ToString[rawData[[idIndex,2]]]],{"#"->"","-"->""}],glass=rawData[[glassIndex,2]],(*physical diameter*)diameter=2*rawData[[semiDiameterIndex,2]],(*center thickness*)centerThickness=rawData[[centerThicknessIndex,2]],(*radius of the first surface*)R1=If[Abs[rawData[[firstCurvatureIndex,2]]]>10^-12,N[Rationalize[1/rawData[[firstCurvatureIndex,2]],0.0001]],\[Infinity]],(*conical of the first surface*)k1=If[firstConicalIndex>0,rawData[[firstConicalIndex,2]],0],(*radius of the second surface*)R2=If[Abs[rawData[[secondCurvatureIndex,2]]]>10^-12,N[Rationalize[1/rawData[[secondCurvatureIndex,2]],0.0001]],\[Infinity]],(*conical of the second surface*)k2=If[secondConicalIndex>0,rawData[[secondConicalIndex,2]],0]},name->With[{firstSurfaceAspheric=(firstSurfaceAsphericCoefficients!={0,0,0,0,0,0,0})||Abs[k1]>= 10^-12,secondSurfaceAspheric=(secondSurfaceAsphericCoefficients!={0,0,0,0,0,0,0})||Abs[k2]>= 10^-12},Which[firstSurfaceAspheric&&secondSurfaceAspheric,HoldForm[OpenRayTracer`createBiAsphericLens[0,R1,k1,firstSurfaceAsphericCoefficients,R2,k2,secondSurfaceAsphericCoefficients,centerThickness, diameter,glass,OpenRayTracer`PartId->name,OpenRayTracer`GlassCatalogueData->OpenRayTracer`GlassCatalogueData]],firstSurfaceAspheric,HoldForm[OpenRayTracer`createAsphericLens[0,R1,k1,firstSurfaceAsphericCoefficients,R2,centerThickness, diameter,glass,OpenRayTracer`PartId->name,OpenRayTracer`GlassCatalogueData->OpenRayTracer`GlassCatalogueData]],
secondSurfaceAspheric,With[{R2Mins=-R2,R1Minus=-R1,secondSurfaceAsphericCoefficientsMinus=-secondSurfaceAsphericCoefficients},HoldForm[OpenRayTracer`createAsphericLens[0,R2Mins,k2,secondSurfaceAsphericCoefficientsMinus,R1Minus,centerThickness, diameter,glass,Reverse->True,OpenRayTracer`PartId->name,OpenRayTracer`GlassCatalogueData->OpenRayTracer`GlassCatalogueData]]],
True,HoldForm[createSphericalLens[0,R1,R2,centerThickness, diameter,glass,OpenRayTracer`PartId->name,OpenRayTracer`GlassCatalogueData->OpenRayTracer`GlassCatalogueData]]]]]]]]]]]]]


readThorlabsZemaxSingletAsphericalLensFile[file_,opts:OptionsPattern[readZemaxSingletAsphericalLensFile]]:=readZemaxSingletAsphericalLensFile[file,"DIAM",opts]


readEdmundOpticsZemaxSingletAsphericalLensFile[file_,opts:OptionsPattern[readZemaxSingletAsphericalLensFile]]:=readZemaxSingletAsphericalLensFile[file,"MEMA",opts]


(* ::Text:: *)
(*Thorlabs aspheric*)


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletAsphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\DiffractionLimitedAsphericLenesesUncoated"]]]*)thorlabsDiffractionLimitedPolishedAsphericLenses=<|"AL1225G"->OpenRayTracer`createAsphericLens[0,12.986842105263158`,-1.0090178704077186`,{0.000025009027712758412`,2.2832711161751136`*^-8,1.7742744498663107`*^-11,0,0,0,0},\[Infinity],3.8`,12.5`,"N-BK7",OpenRayTracer`PartId->"AL1225G"],"AL2550G"->OpenRayTracer`createAsphericLens[0,25.973684210526315`,-1.000983755829444`,{3.040788224594284`*^-6,7.205947606261041`*^-10,2.32917540323789`*^-14,0,0,0,0},\[Infinity],5.8`,25.`,"N-BK7",OpenRayTracer`PartId->"AL2550G"],"AL50100G"->OpenRayTracer`createAsphericLens[0,51.947169811320755`,-1.3233414018499627`,{6.662026992552446`*^-7,4.080039980427164`*^-12,2.249389518037379`*^-15,-1.0645514650035663`*^-19,0,0,0},\[Infinity],9.8`,50,"N-BK7",OpenRayTracer`PartId->"AL50100G"]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletAsphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\PolishedAsphericLensesUnmounted"]]]*)thorlabsPolishedAsphericLenses=<|"AL100100"->OpenRayTracer`createAsphericLens[0,51.12`,-1.023`,{4.427892654967`*^-7,2.871501946835`*^-11,1.920119455977`*^-15,9.212480300265`*^-20,-1.605226412147`*^-24,-5.863837411815`*^-28,-3.082191437463`*^-31},\[Infinity],36,100,"N-BK7",OpenRayTracer`PartId->"AL100100",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA"}],"AL100200"->OpenRayTracer`createAsphericLens[0,102.24`,-1.`,{4.964600303484`*^-8,7.401787207826`*^-13,9.414170279431`*^-18,0,0,0,0},\[Infinity],19.`,100.`,"N-BK7",OpenRayTracer`PartId->"AL100200",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA"}],"AL108"->OpenRayTracer`createAsphericLens[0,6.21505376344086`,-1.`,{0.0002005941405611`,-1.049843112924`*^-7,-1.126355561455`*^-8,-1.020122114068`*^-10,8.400226227819`*^-13,4.636236347837`*^-15,1.206294645345`*^-16},\[Infinity],3.7`,10.`,"S-LAH64",OpenRayTracer`PartId->"AL108",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CORNING","MISC","OHARA"}],"AL1210"->OpenRayTracer`createAsphericLens[0,7.77`,-1.`,{0.00009846431852383`,-6.990585106497`*^-8,-2.387499355499`*^-9,-1.132858260463`*^-11,8.725543751897`*^-14,2.896731308378`*^-16,1.763211202269`*^-18},\[Infinity],4.25`,12.5`,"S-LAH64",OpenRayTracer`PartId->"AL1210",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA","OHARA"}],"AL1225"->OpenRayTracer`createAsphericLens[0,12.78`,-0.6`,{1.842989761529`*^-6,-3.817225154451`*^-9,-2.434545726277`*^-11,3.173049559587`*^-14,-3.700237030871`*^-15,6.510782069663`*^-17,-4.960414659811`*^-19},\[Infinity],4.`,12.5`,"N-BK7",OpenRayTracer`PartId->"AL1225",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA","OHARA","HERAEUS"}],"AL1512"->OpenRayTracer`createAsphericLens[0,9.32`,-1.`,{0.00005759869708203`,-2.503422017417`*^-8,-6.751998848424`*^-10,-2.001847382922`*^-12,3.868482820049`*^-15,1.24474770674`*^-16,-3.659331007393`*^-19},\[Infinity],5.28`,15.`,"S-LAH64",OpenRayTracer`PartId->"AL1512",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA","OHARA"}],"AL1815"->OpenRayTracer`createAsphericLens[0,11.65`,-1.1`,{0.0000369067205091`,-1.285461192617`*^-8,-1.400167659754`*^-10,-2.51311664812`*^-13,5.01789877647`*^-16,5.855871513042`*^-18,-1.127794407419`*^-20},\[Infinity],6.2`,18.`,"S-LAH64",OpenRayTracer`PartId->"AL1815",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA","OHARA"}],"AL2018"->OpenRayTracer`createAsphericLens[0,13.98`,-1.4`,{0.00003488237577287`,-2.32070575136`*^-8,-1.536027109498`*^-11,-8.166799824776`*^-14,1.566993480861`*^-16,2.264126882812`*^-19,0},\[Infinity],7.1`,20.`,"S-LAH64",OpenRayTracer`PartId->"AL2018",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA","OHARA"}],"AL2520"->OpenRayTracer`createAsphericLens[0,15.54`,-1.35`,{0.00002361813418595`,-1.130307881793`*^-8,-1.111390564998`*^-11,-2.398171396799`*^-14,3.035790951341`*^-17,1.366081467742`*^-19,-1.888158702521`*^-22},\[Infinity],7.6`,25.`,"S-LAH64",OpenRayTracer`PartId->"AL2520",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA","OHARA"}],"AL2550"->OpenRayTracer`createAsphericLens[0,25.56`,-1.01`,{3.270395763922`*^-6,7.72053347503`*^-10,1.630472733467`*^-13,0,0,0,0},\[Infinity],6.`,25.`,"N-BK7",OpenRayTracer`PartId->"AL2550",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA"}],"AL3026"->OpenRayTracer`createAsphericLens[0,20.2`,-1.`,{5.414454156356`*^-6,-8.041331544432`*^-10,-2.987118945351`*^-12,-1.491792710669`*^-15,1.377731660511`*^-18,4.425802319293`*^-21,-3.492766752395`*^-24},\[Infinity],9.65`,30.`,"S-LAH64",OpenRayTracer`PartId->"AL3026",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA","OHARA"}],"AL4532"->OpenRayTracer`createAsphericLens[0,24.86`,-1.`,{3.023059476872`*^-6,-1.798782337655`*^-10,-8.040863803154`*^-13,1.35145424092`*^-16,-9.371822841971`*^-19,1.684930035933`*^-21,-8.714644822103`*^-25},\[Infinity],13.9`,45.`,"S-LAH64",OpenRayTracer`PartId->"AL4532",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA","OHARA"}],"AL50100"->OpenRayTracer`createAsphericLens[0,51.12`,-0.575`,{-4.836626409504`*^-11,-8.575691513714`*^-12,-2.013822272477`*^-15,-4.597797128348`*^-19,0,0,0},\[Infinity],10.`,50.`,"N-BK7",OpenRayTracer`PartId->"AL50100",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA"}],"AL5040"->OpenRayTracer`createAsphericLens[0,31.075`,-0.744`,{4.366522135427`*^-7,-2.271359087825`*^-10,-1.704217360999`*^-13,-3.680934404878`*^-17,8.944345914633`*^-21,1.850118782323`*^-23,-6.270433312134`*^-27},\[Infinity],15.5`,50.`,"S-LAH64",OpenRayTracer`PartId->"AL5040",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA","OHARA"}],"AL75150"->OpenRayTracer`createAsphericLens[0,76.68`,-0.675`,{2.770921913101`*^-8,6.418186000505`*^-13,-1.572401434381`*^-17,-2.776876837231`*^-21,-2.590162013724`*^-25,0,0},\[Infinity],15.`,75.`,"N-BK7",OpenRayTracer`PartId->"AL75150",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA"}],"AL7560"->OpenRayTracer`createAsphericLens[0,30.67`,-0.905`,{1.636595786587`*^-6,4.129299147209`*^-10,9.354037461943`*^-14,2.453064915515`*^-17,-4.606737758378`*^-21,5.57723982822`*^-24,-2.635821133659`*^-27},\[Infinity],35.5`,75.`,"N-BK7",OpenRayTracer`PartId->"AL7560",OpenRayTracer`GlassCatalogueData->{"SCHOTT","HOYA"}]|>;


(* ::Text:: *)
(*Thorlabs spherical lenses*)


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\CaF2_NegativeMeniscus"]]]*)thorlabsCaF2NegativeMeniscus=<|"LF5048"->OpenRayTracer`createSphericalLens[0,218.3`,105.1`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LF5048",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5067"->OpenRayTracer`createSphericalLens[0,95.6`,7.5`,2,12.7`,"CAF2",OpenRayTracer`PartId->"LF5067",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5154"->OpenRayTracer`createSphericalLens[0,33.7`,16.2`,3.`,12.7`,"CAF2",OpenRayTracer`PartId->"LF5154",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5175"->OpenRayTracer`createSphericalLens[0,64.1`,30.8`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LF5175",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5394"->OpenRayTracer`createSphericalLens[0,438.1`,211.1`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LF5394",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5414"->OpenRayTracer`createSphericalLens[0,68.5`,15.5`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LF5414",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5469"->OpenRayTracer`createSphericalLens[0,308.3`,15.5`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LF5469",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5515"->OpenRayTracer`createSphericalLens[0,42.`,20.2`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LF5515",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5807"->OpenRayTracer`createSphericalLens[0,328.2`,158.1`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LF5807",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5847"->OpenRayTracer`createSphericalLens[0,20.4`,9.8`,3.`,12.7`,"CAF2",OpenRayTracer`PartId->"LF5847",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5895"->OpenRayTracer`createSphericalLens[0,30.8`,14.8`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LF5895",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LF5961"->OpenRayTracer`createSphericalLens[0,86.2`,41.4`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LF5961",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\CaF2_PositiveMeniscus"]]]*)thorlabsCaF2PositiveMeniscus=<|"LE5183"->OpenRayTracer`createSphericalLens[0,15.`,146.9`,8.9`,25.4`,"CAF2",OpenRayTracer`PartId->"LE5183",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5234"->OpenRayTracer`createSphericalLens[0,20.`,49.1`,3.`,12.7`,"CAF2",OpenRayTracer`PartId->"LE5234",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5243"->OpenRayTracer`createSphericalLens[0,15.`,52.8`,3.`,12.7`,"CAF2",OpenRayTracer`PartId->"LE5243",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5382"->OpenRayTracer`createSphericalLens[0,35.`,78.6`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LE5382",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5414"->OpenRayTracer`createSphericalLens[0,40.`,75.9`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LE5414",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5656"->OpenRayTracer`createSphericalLens[0,150.`,290.8`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LE5656",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5714"->OpenRayTracer`createSphericalLens[0,250.`,638.5`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LE5714",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5801"->OpenRayTracer`createSphericalLens[0,15.5`,55.2`,7.1`,25.4`,"CAF2",OpenRayTracer`PartId->"LE5801",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5802"->OpenRayTracer`createSphericalLens[0,15.5`,28.`,5.6`,25.4`,"CAF2",OpenRayTracer`PartId->"LE5802",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5803"->OpenRayTracer`createSphericalLens[0,20.`,36.8`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LE5803",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5838"->OpenRayTracer`createSphericalLens[0,7.5`,72.4`,4.7`,12.7`,"CAF2",OpenRayTracer`PartId->"LE5838",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LE5990"->OpenRayTracer`createSphericalLens[0,125.`,317.8`,4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LE5990",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\CaF2_BiConcave"]]]*)thorlabsCaF2BiConcave=<|"LD5138"->OpenRayTracer`createSphericalLens[0,-26.4`,26.4`,2.5`,25.4`,"CAF2",OpenRayTracer`PartId->"LD5138",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LD5313"->OpenRayTracer`createSphericalLens[0,-43.83`,43.83`,3.`,25.4`,"CAF2",OpenRayTracer`PartId->"LD5313",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LD5451"->OpenRayTracer`createSphericalLens[0,-13.38`,13.38`,2.5`,12.7`,"CAF2",OpenRayTracer`PartId->"LD5451",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LD5788"->OpenRayTracer`createSphericalLens[0,-22.13`,22.13`,3.`,12.7`,"CAF2",OpenRayTracer`PartId->"LD5788",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\CaF2_BiConvex"]]]*)thorlabsCaF2BiConvex=<|"LB5247"->OpenRayTracer`createSphericalLens[0,64.38461538461539`,-64.38461538461539`,4.529957259143`,25.4`,"CAF2",OpenRayTracer`PartId->"LB5247",OpenRayTracer`GlassCatalogueData->{"INFRARED"}],"LB5284"->OpenRayTracer`createSphericalLens[0,42.475247524752476`,-42.475247524752476`,5.886157372187`,25.4`,"CAF2",OpenRayTracer`PartId->"LB5284",OpenRayTracer`GlassCatalogueData->{"INFRARED"}],"LB5454"->OpenRayTracer`createSphericalLens[0,173.09448818897638`,-173.09448818897638`,2.93306070388`,25.4`,"CAF2",OpenRayTracer`PartId->"LB5454",OpenRayTracer`GlassCatalogueData->{"INFRARED"}],"LB5552"->OpenRayTracer`createSphericalLens[0,86.17829457364341`,-86.17829457364341`,3.881856806088`,25.4`,"CAF2",OpenRayTracer`PartId->"LB5552",OpenRayTracer`GlassCatalogueData->{"INFRARED"}],"LB5766"->OpenRayTracer`createSphericalLens[0,12.104651162790697`,-12.104651162790697`,5.598591922558`,12.7`,"CAF2",OpenRayTracer`PartId->"LB5766",OpenRayTracer`GlassCatalogueData->{"INFRARED"}],"LB5774"->OpenRayTracer`createSphericalLens[0,20.232558139534884`,-20.232558139534884`,10.9649129683`,25.4`,"CAF2",OpenRayTracer`PartId->"LB5774",OpenRayTracer`GlassCatalogueData->{"INFRARED"}],"LB5864"->OpenRayTracer`createSphericalLens[0,34.21848739495798`,-34.21848739495798`,3.188704495222`,12.7`,"CAF2",OpenRayTracer`PartId->"LB5864",OpenRayTracer`GlassCatalogueData->{"INFRARED"}],"LB5922"->OpenRayTracer`createSphericalLens[0,16.641025641025642`,-16.641025641025642`,4.518347143421`,12.7`,"CAF2",OpenRayTracer`PartId->"LB5922",OpenRayTracer`GlassCatalogueData->{"INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\CaF2_PlanoConcave"]]]*)thorlabsCaF2PlanoConcave=<|"LC5269"->OpenRayTracer`createSphericalLens[0,-17.35`,\[Infinity],2,25.4`,"CAF2",OpenRayTracer`PartId->"LC5269",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC5289"->OpenRayTracer`createSphericalLens[0,-43.38`,\[Infinity],3.`,25.4`,"CAF2",OpenRayTracer`PartId->"LC5289",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC5401"->OpenRayTracer`createSphericalLens[0,-32.54`,\[Infinity],2.5`,25.4`,"CAF2",OpenRayTracer`PartId->"LC5401",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC5749"->OpenRayTracer`createSphericalLens[0,-10.84`,\[Infinity],2.5`,12.7`,"CAF2",OpenRayTracer`PartId->"LC5749",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC5893"->OpenRayTracer`createSphericalLens[0,-216.94`,\[Infinity],4.`,25.4`,"CAF2",OpenRayTracer`PartId->"LC5893",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC5919"->OpenRayTracer`createSphericalLens[0,-7.8`,\[Infinity],2,12.7`,"CAF2",OpenRayTracer`PartId->"LC5919",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC5952"->OpenRayTracer`createSphericalLens[0,-86.77`,\[Infinity],3.5`,25.4`,"CAF2",OpenRayTracer`PartId->"LC5952",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*Association[Join[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\CaF2_PlanoConvex"]],Map[readThorlabsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\CaF2_PlanoConvex\\Unicode"]]]]*)thorlabsCaF2PlanoConvex=<|"LA5012"->OpenRayTracer`createSphericalLens[0,65.08`,\[Infinity],3.25`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5012",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5042"->OpenRayTracer`createSphericalLens[0,32.54`,\[Infinity],4.58`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5042",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5183"->OpenRayTracer`createSphericalLens[0,21.69`,\[Infinity],2.45`,12.7`,"CAF2",OpenRayTracer`PartId->"LA5183",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5255"->OpenRayTracer`createSphericalLens[0,108.47`,\[Infinity],2.74`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5255",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5315"->OpenRayTracer`createSphericalLens[0,8.67`,\[Infinity],4.26`,12.7`,"CAF2",OpenRayTracer`PartId->"LA5315",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5370"->OpenRayTracer`createSphericalLens[0,17.35`,\[Infinity],7.52`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5370",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5458"->OpenRayTracer`createSphericalLens[0,34.71`,\[Infinity],2.08`,12.7`,"CAF2",OpenRayTracer`PartId->"LA5458",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5464"->OpenRayTracer`createSphericalLens[0,216.94`,\[Infinity],2.37`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5464",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5714"->OpenRayTracer`createSphericalLens[0,86.77`,\[Infinity],2.93`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5714",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5763"->OpenRayTracer`createSphericalLens[0,21.69`,\[Infinity],6.1`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5763",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5817"->OpenRayTracer`createSphericalLens[0,43.38`,\[Infinity],3.9`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5817",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5835"->OpenRayTracer`createSphericalLens[0,433.88`,\[Infinity],2.18`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5835",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5956"->OpenRayTracer`createSphericalLens[0,325.41`,\[Infinity],2.24`,25.4`,"CAF2",OpenRayTracer`PartId->"LA5956",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA5010"->OpenRayTracer`createSphericalLens[0,43.38`,\[Infinity],2.47`,12.7`,"CAF2",OpenRayTracer`PartId->"LA5010",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA5025"->OpenRayTracer`createSphericalLens[0,10.85`,\[Infinity],4.05`,12.7`,"CAF2",OpenRayTracer`PartId->"LA5025",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA5030"->OpenRayTracer`createSphericalLens[0,13.02`,\[Infinity],3.65`,12.7`,"CAF2",OpenRayTracer`PartId->"LA5030",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA5040"->OpenRayTracer`createSphericalLens[0,17.35`,\[Infinity],3.2`,12.7`,"CAF2",OpenRayTracer`PartId->"LA5040",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA5200"->OpenRayTracer`createSphericalLens[0,433.84`,\[Infinity],3.7441842732719266`,50.8`,"CAF2",OpenRayTracer`PartId->"LA5200",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA5210"->OpenRayTracer`createSphericalLens[0,43.38383838383838`,\[Infinity],11.21`,50.8`,"CAF2",OpenRayTracer`PartId->"LA5210",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA5215"->OpenRayTracer`createSphericalLens[0,65.08`,\[Infinity],8.161335128359209`,50.8`,"CAF2",OpenRayTracer`PartId->"LA5215",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA5220"->OpenRayTracer`createSphericalLens[0,86.77`,\[Infinity],6.8`,50.8`,"CAF2",OpenRayTracer`PartId->"LA5220",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA5230"->OpenRayTracer`createSphericalLens[0,130.15`,\[Infinity],5.5`,50.8`,"CAF2",OpenRayTracer`PartId->"LA5230",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA5250"->OpenRayTracer`createSphericalLens[0,216.92`,\[Infinity],4.49`,50.8`,"CAF2",OpenRayTracer`PartId->"LA5250",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\FusedSilica_NegativeMeniscus"]]]*)thorlabsFusedSilicaNegativeMeniscus=<|"LF4348"->OpenRayTracer`createSphericalLens[0,150.`,71.35`,3.5`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LF4348",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF4370"->OpenRayTracer`createSphericalLens[0,150.`,46.96`,3.`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LF4370",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF4624"->OpenRayTracer`createSphericalLens[0,150.`,56.61`,3.5`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LF4624",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF4706"->OpenRayTracer`createSphericalLens[0,200.`,106.39`,3.5`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LF4706",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF4938"->OpenRayTracer`createSphericalLens[0,150.`,34.98`,3.`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LF4938",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF4986"->OpenRayTracer`createSphericalLens[0,300.`,180.82`,4.`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LF4986",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\FusedSilica_PositiveMeniscus"]]]*)thorlabsFusedSilicaPositiveMeniscus=<|"LE4125"->OpenRayTracer`createSphericalLens[0,46.53`,135.25`,7.84`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LE4125",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4150"->OpenRayTracer`createSphericalLens[0,165.37`,582.75`,5.`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LE4150",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4173"->OpenRayTracer`createSphericalLens[0,31.`,91.17`,4.`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LE4173",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4197"->OpenRayTracer`createSphericalLens[0,47.64`,149.76`,3.18`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LE4197",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4329"->OpenRayTracer`createSphericalLens[0,97.61`,330.58`,2.59`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LE4329",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4412"->OpenRayTracer`createSphericalLens[0,30.55`,80.94`,10.65`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LE4412",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4467"->OpenRayTracer`createSphericalLens[0,64.24`,209.81`,2.88`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LE4467",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4484"->OpenRayTracer`createSphericalLens[0,166.75`,603.39`,2.35`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LE4484",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4560"->OpenRayTracer`createSphericalLens[0,63.02`,193.41`,6.58`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LE4560",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4822"->OpenRayTracer`createSphericalLens[0,356.69`,1580.24`,5.`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LE4822",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4950"->OpenRayTracer`createSphericalLens[0,348.`,1425.69`,2.18`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LE4950",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE4984"->OpenRayTracer`createSphericalLens[0,97.61`,327.59`,5.36`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LE4984",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*one entry added manuually,Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\FusedSilicaBiConvex"]]]*)thorlabsFusedSilicaBiConvex=<|"LB4821"->OpenRayTracer`createSphericalLens[0,90.4,-90.4,10.3,50.8,"F_SILICA",OpenRayTracer`PartId->"LB4821",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4003"->OpenRayTracer`createSphericalLens[0,27.07`,-27.07`,3.31`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LB4003",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4030"->OpenRayTracer`createSphericalLens[0,35.72`,-35.72`,6.67`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4030",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4096"->OpenRayTracer`createSphericalLens[0,45.09`,-45.09`,5.65`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4096",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4140"->OpenRayTracer`createSphericalLens[0,136.79`,-136.79`,7.76`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LB4140",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4223"->OpenRayTracer`createSphericalLens[0,689.76`,-689.76`,2.23`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4223",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4265"->OpenRayTracer`createSphericalLens[0,137.52`,-137.52`,3.18`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4265",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4280"->OpenRayTracer`createSphericalLens[0,8.77`,-8.77`,2.56`,6.`,"F_SILICA",OpenRayTracer`PartId->"LB4280",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4282"->OpenRayTracer`createSphericalLens[0,183.57`,-183.57`,2.88`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4282",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4293"->OpenRayTracer`createSphericalLens[0,919.57`,-919.57`,3.7`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LB4293",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4330"->OpenRayTracer`createSphericalLens[0,68.31`,-68.31`,4.38`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4330",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4374"->OpenRayTracer`createSphericalLens[0,919.81`,-919.81`,2.18`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4374",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4453"->OpenRayTracer`createSphericalLens[0,459.7`,-459.7`,2.35`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4453",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4537"->OpenRayTracer`createSphericalLens[0,13.44`,-13.44`,2.18`,6.`,"F_SILICA",OpenRayTracer`PartId->"LB4537",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4545"->OpenRayTracer`createSphericalLens[0,275.63`,-275.63`,2.59`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4545",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4553"->OpenRayTracer`createSphericalLens[0,66.98`,-66.98`,12.51`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LB4553",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4592"->OpenRayTracer`createSphericalLens[0,52.63`,-52.63`,15.57`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LB4592",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4700"->OpenRayTracer`createSphericalLens[0,36.34`,-36.34`,2.92`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LB4700",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4710"->OpenRayTracer`createSphericalLens[0,275.2`,-275.2`,5.35`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LB4710",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4738"->OpenRayTracer`createSphericalLens[0,18.08`,-18.08`,2,6.`,"F_SILICA",OpenRayTracer`PartId->"LB4738",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4743"->OpenRayTracer`createSphericalLens[0,8.83`,-8.83`,2.22`,5.`,"F_SILICA",OpenRayTracer`PartId->"LB4743",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4837"->OpenRayTracer`createSphericalLens[0,229.61`,-229.61`,2.7`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4837",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4842"->OpenRayTracer`createSphericalLens[0,182.99`,-182.99`,6.54`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LB4842",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4854"->OpenRayTracer`createSphericalLens[0,17.72`,-17.72`,4.15`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LB4854",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4879"->OpenRayTracer`createSphericalLens[0,30.98`,-30.98`,7.44`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4879",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4910"->OpenRayTracer`createSphericalLens[0,459.38`,-459.38`,4.41`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LB4910",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4913"->OpenRayTracer`createSphericalLens[0,114.48`,-114.48`,3.41`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4913",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4915"->OpenRayTracer`createSphericalLens[0,45.58`,-45.58`,2.69`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LB4915",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4941"->OpenRayTracer`createSphericalLens[0,91.41`,-91.41`,3.77`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LB4941",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB4972"->OpenRayTracer`createSphericalLens[0,229.11`,-229.11`,5.82`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LB4972",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\FusedSilicaPlanoConcave"]]]*)thorlabsFusedSilicaPlanoConcave=<|"LC4210"->OpenRayTracer`createSphericalLens[0,-11.5`,\[Infinity],3.`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LC4210",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4232"->OpenRayTracer`createSphericalLens[0,-46.`,\[Infinity],3.5`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LC4232",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4252"->OpenRayTracer`createSphericalLens[0,-13.8`,\[Infinity],3.`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LC4252",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4291"->OpenRayTracer`createSphericalLens[0,-5.52`,\[Infinity],2,8.`,"F_SILICA",OpenRayTracer`PartId->"LC4291",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4357"->OpenRayTracer`createSphericalLens[0,-23.`,\[Infinity],3.5`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LC4357",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4413"->OpenRayTracer`createSphericalLens[0,-34.5`,\[Infinity],3.5`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LC4413",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4513"->OpenRayTracer`createSphericalLens[0,-34.5`,\[Infinity],3.5`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LC4513",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4573"->OpenRayTracer`createSphericalLens[0,-4.6`,\[Infinity],2,6.`,"F_SILICA",OpenRayTracer`PartId->"LC4573",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4796"->OpenRayTracer`createSphericalLens[0,-13.8`,\[Infinity],3.`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LC4796",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4888"->OpenRayTracer`createSphericalLens[0,-46.`,\[Infinity],3.5`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LC4888",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4918"->OpenRayTracer`createSphericalLens[0,-92.01`,\[Infinity],4.`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LC4918",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC4924"->OpenRayTracer`createSphericalLens[0,-9.2`,\[Infinity],2,12.7`,"F_SILICA",OpenRayTracer`PartId->"LC4924",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*Association[Join[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\FusedSilicaPlanoConvex"]],Map[readThorlabsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\FusedSilicaPlanoConvex\\Unicode"]]]]*)thorlabsFusedSilicaPlanoConvex=<|"LA4052"->OpenRayTracer`createSphericalLens[0,16.1`,\[Infinity],8.2`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4052",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4078"->OpenRayTracer`createSphericalLens[0,34.5`,\[Infinity],14.14`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4078",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4102"->OpenRayTracer`createSphericalLens[0,92.01`,\[Infinity],2.88`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4102",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4130"->OpenRayTracer`createSphericalLens[0,18.4`,\[Infinity],2.93`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LA4130",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4148"->OpenRayTracer`createSphericalLens[0,23.`,\[Infinity],5.82`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4148",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4158"->OpenRayTracer`createSphericalLens[0,115.02`,\[Infinity],2.7`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4158",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4184"->OpenRayTracer`createSphericalLens[0,230.04`,\[Infinity],2.35`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4184",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4194"->OpenRayTracer`createSphericalLens[0,9.2`,\[Infinity],2,6.`,"F_SILICA",OpenRayTracer`PartId->"LA4194",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4236"->OpenRayTracer`createSphericalLens[0,57.51`,\[Infinity],3.41`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4236",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4246"->OpenRayTracer`createSphericalLens[0,230.04`,\[Infinity],6.07`,75.`,"F_SILICA",OpenRayTracer`PartId->"LA4246",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4249"->OpenRayTracer`createSphericalLens[0,4.6`,\[Infinity],2.23`,5.`,"F_SILICA",OpenRayTracer`PartId->"LA4249",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4280"->OpenRayTracer`createSphericalLens[0,4.6`,\[Infinity],2.61`,6.`,"F_SILICA",OpenRayTracer`PartId->"LA4280",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4306"->OpenRayTracer`createSphericalLens[0,18.4`,\[Infinity],7.08`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4306",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4327"->OpenRayTracer`createSphericalLens[0,34.5`,\[Infinity],2.38`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LA4327",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4337"->OpenRayTracer`createSphericalLens[0,460.08`,\[Infinity],3.7`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4337",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4372"->OpenRayTracer`createSphericalLens[0,69.01`,\[Infinity],14.07`,75.`,"F_SILICA",OpenRayTracer`PartId->"LA4372",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4380"->OpenRayTracer`createSphericalLens[0,46.`,\[Infinity],3.78`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4380",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4384"->OpenRayTracer`createSphericalLens[0,41.4`,\[Infinity],26.84`,75.`,"F_SILICA",OpenRayTracer`PartId->"LA4384",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4464"->OpenRayTracer`createSphericalLens[0,27.6`,\[Infinity],19.79`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4464",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4538"->OpenRayTracer`createSphericalLens[0,115.02`,\[Infinity],5.83`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4538",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4545"->OpenRayTracer`createSphericalLens[0,46.`,\[Infinity],10.64`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4545",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4579"->OpenRayTracer`createSphericalLens[0,138.02`,\[Infinity],2.58`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4579",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4600"->OpenRayTracer`createSphericalLens[0,46.`,\[Infinity],2.24`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LA4600",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4647"->OpenRayTracer`createSphericalLens[0,9.2`,\[Infinity],4.34`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LA4647",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4663"->OpenRayTracer`createSphericalLens[0,460.08`,\[Infinity],2.17`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4663",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4716"->OpenRayTracer`createSphericalLens[0,345.06`,\[Infinity],2.23`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4716",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4725"->OpenRayTracer`createSphericalLens[0,34.5`,\[Infinity],4.42`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4725",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4745"->OpenRayTracer`createSphericalLens[0,345.06`,\[Infinity],3.93`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4745",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4765"->OpenRayTracer`createSphericalLens[0,23.`,\[Infinity],2.69`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LA4765",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4782"->OpenRayTracer`createSphericalLens[0,230.04`,\[Infinity],4.4`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4782",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4795"->OpenRayTracer`createSphericalLens[0,92.01`,\[Infinity],10.98`,75.`,"F_SILICA",OpenRayTracer`PartId->"LA4795",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4855"->OpenRayTracer`createSphericalLens[0,138.02`,\[Infinity],5.35`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4855",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4874"->OpenRayTracer`createSphericalLens[0,69.01`,\[Infinity],3.17`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4874",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4904"->OpenRayTracer`createSphericalLens[0,69.01`,\[Infinity],7.84`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4904",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4917"->OpenRayTracer`createSphericalLens[0,6.9`,\[Infinity],2.18`,6.`,"F_SILICA",OpenRayTracer`PartId->"LA4917",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4924"->OpenRayTracer`createSphericalLens[0,80.51`,\[Infinity],3.`,25.4`,"F_SILICA",OpenRayTracer`PartId->"LA4924",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4936"->OpenRayTracer`createSphericalLens[0,13.8`,\[Infinity],3.34`,12.7`,"F_SILICA",OpenRayTracer`PartId->"LA4936",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4966"->OpenRayTracer`createSphericalLens[0,13.8`,\[Infinity],1.82`,6.`,"F_SILICA",OpenRayTracer`PartId->"LA4966",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4984"->OpenRayTracer`createSphericalLens[0,92.01`,\[Infinity],6.57`,50.8`,"F_SILICA",OpenRayTracer`PartId->"LA4984",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA4024"->OpenRayTracer`createSphericalLens[0,1.8338870431893688`,\[Infinity],1,2,"F_SILICA",OpenRayTracer`PartId->"LA4024",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA4026"->OpenRayTracer`createSphericalLens[0,2.7507886435331232`,\[Infinity],1,2,"F_SILICA",OpenRayTracer`PartId->"LA4026",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA4036"->OpenRayTracer`createSphericalLens[0,2.7507886435331232`,\[Infinity],1.5`,3.`,"F_SILICA",OpenRayTracer`PartId->"LA4036",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}],"LA4039"->OpenRayTracer`createSphericalLens[0,4.126126126126126`,\[Infinity],1.5`,3.`,"F_SILICA",OpenRayTracer`PartId->"LA4039",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NSF11_BiConcave"]]]*)thorlabsNSF11BiConcave=<|"LD2060"->OpenRayTracer`createSphericalLens[0,-23.99`,23.99`,3.`,12.7`,"N-SF11",OpenRayTracer`PartId->"LD2060",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LD2297"->OpenRayTracer`createSphericalLens[0,-39.57`,39.57`,3.`,25.4`,"N-SF11",OpenRayTracer`PartId->"LD2297",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LD2568"->OpenRayTracer`createSphericalLens[0,-14.43`,14.43`,2,9.`,"N-SF11",OpenRayTracer`PartId->"LD2568",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LD2746"->OpenRayTracer`createSphericalLens[0,-9.66`,9.66`,1.5`,6.`,"N-SF11",OpenRayTracer`PartId->"LD2746",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NSF11_PlanoConcave"]]]*)thorlabsNSF11PlanoConcave=<|"LC2067"->OpenRayTracer`createSphericalLens[0,-7.`,\[Infinity],2,9.`,"N-SF11",OpenRayTracer`PartId->"LC2067",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC2265"->OpenRayTracer`createSphericalLens[0,-11.67`,\[Infinity],3.`,12.7`,"N-SF11",OpenRayTracer`PartId->"LC2265",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC2632"->OpenRayTracer`createSphericalLens[0,-9.34`,\[Infinity],2,6.`,"N-SF11",OpenRayTracer`PartId->"LC2632",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC2679"->OpenRayTracer`createSphericalLens[0,-23.35`,\[Infinity],3.5`,25.4`,"N-SF11",OpenRayTracer`PartId->"LC2679",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC2873"->OpenRayTracer`createSphericalLens[0,-14.01`,\[Infinity],2.5`,9.`,"N-SF11",OpenRayTracer`PartId->"LC2873",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC2969"->OpenRayTracer`createSphericalLens[0,-4.67`,\[Infinity],1.5`,6.`,"N-SF11",OpenRayTracer`PartId->"LC2969",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NBK7_BestForm"]]]*)thorlabsNBK7BestForm=<|"LBF254-040"->OpenRayTracer`createSphericalLens[0,24.02`,-134.6`,6.5`,25.4`,"N-BK7",OpenRayTracer`PartId->"LBF254-040",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LBF254-050"->OpenRayTracer`createSphericalLens[0,30.06`,-172.`,6.5`,25.4`,"N-BK7",OpenRayTracer`PartId->"LBF254-050",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LBF254-075"->OpenRayTracer`createSphericalLens[0,44.5`,-289.`,5.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LBF254-075",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LBF254-100"->OpenRayTracer`createSphericalLens[0,60.02`,-353.3`,4.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LBF254-100",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LBF254-150"->OpenRayTracer`createSphericalLens[0,89.35`,-570.49`,4.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LBF254-150",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LBF254-200"->OpenRayTracer`createSphericalLens[0,121.5`,-684.5`,4.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LBF254-200",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*data downloaded on 12.06.2020 from the Thorlabs website,two entries added manually due to lack of Zemax files,Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NBK7_NegativeMeniscus"]]]*)thorlabsNBK7NegativeMeniscus=<|"LF1822"->OpenRayTracer`createSphericalLens[0,100.0,33.7,3.0,25.4`,"N-BK7",OpenRayTracer`PartId->"LF1822",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF1988"->OpenRayTracer`createSphericalLens[0,250.0,126.3,3.0,25.4`,"N-BK7",OpenRayTracer`PartId->"LF1988",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF1015"->OpenRayTracer`createSphericalLens[0,250.`,95.11`,3.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LF1015",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF1097"->OpenRayTracer`createSphericalLens[0,100.`,50.22`,3.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LF1097",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF1141"->OpenRayTracer`createSphericalLens[0,500.`,253.19`,3.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LF1141",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF1544"->OpenRayTracer`createSphericalLens[0,250.`,112.48`,3.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LF1544",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LF1547"->OpenRayTracer`createSphericalLens[0,100.`,43.14`,3.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LF1547",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*data downloaded on 12.06.2020 from the Thorlabs website,Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NBK7_PositiveMeniscus"]]]*)thorlabsNBK7PositiveMeniscus=<|"LE1015"->OpenRayTracer`createSphericalLens[0,65.16`,171.6`,6.18`,50.8`,"N-BK7",OpenRayTracer`PartId->"LE1015",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE1076"->OpenRayTracer`createSphericalLens[0,30.34`,65.8`,9.7`,50.8`,"N-BK7",OpenRayTracer`PartId->"LE1076",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE1104"->OpenRayTracer`createSphericalLens[0,49.06`,131.56`,3.05`,25.4`,"N-BK7",OpenRayTracer`PartId->"LE1104",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE1156"->OpenRayTracer`createSphericalLens[0,40.6`,106.9`,3.26`,25.4`,"N-BK7",OpenRayTracer`PartId->"LE1156",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE1202"->OpenRayTracer`createSphericalLens[0,66.15`,182.19`,2.79`,25.4`,"N-BK7",OpenRayTracer`PartId->"LE1202",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE1234"->OpenRayTracer`createSphericalLens[0,32.14`,82.23`,3.59`,25.4`,"N-BK7",OpenRayTracer`PartId->"LE1234",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE1418"->OpenRayTracer`createSphericalLens[0,47.87`,119.32`,7.29`,50.8`,"N-BK7",OpenRayTracer`PartId->"LE1418",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE1613"->OpenRayTracer`createSphericalLens[0,82.54`,224.67`,5.53`,50.8`,"N-BK7",OpenRayTracer`PartId->"LE1613",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE1929"->OpenRayTracer`createSphericalLens[0,100.89`,288.2`,2.52`,25.4`,"N-BK7",OpenRayTracer`PartId->"LE1929",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LE1985"->OpenRayTracer`createSphericalLens[0,100.08`,279.12`,5.1`,50.8`,"N-BK7",OpenRayTracer`PartId->"LE1985",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*data downloaded on 12.06.2020 from the Thorlabs website,Association[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NBK7_BiConcave"]]]*)thorlabsNBK7BiConcave=<|"LD1170"->OpenRayTracer`createSphericalLens[0,-77.85`,77.85`,3.5`,25.4`,"N-BK7",OpenRayTracer`PartId->"LD1170",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LD1357"->OpenRayTracer`createSphericalLens[0,-52.09`,52.09`,3.5`,12.7`,"N-BK7",OpenRayTracer`PartId->"LD1357",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LD1464"->OpenRayTracer`createSphericalLens[0,-52.01`,52.01`,3.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LD1464",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LD1613"->OpenRayTracer`createSphericalLens[0,-103.69`,103.69`,4.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LD1613",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*data downloaded on 12.06.2020 from the Thorlabs website,one entry added manually,Association[Join[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NBK7_BiConvex"]]]].one entry added manually due to lack of a Zemax file available for download on teh Thorlab website*)thorlabsNBK7BiConvex=<|"LB1187"->OpenRayTracer`createSphericalLens[0,102.6,-102.6,2.2,12.7`,"N-BK7",OpenRayTracer`PartId->"LB1187",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1014"->OpenRayTracer`createSphericalLens[0,25.15`,-25.15`,3.43`,12.7`,"N-BK7",OpenRayTracer`PartId->"LB1014",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1027"->OpenRayTracer`createSphericalLens[0,40.13`,-40.13`,6.12`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1027",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1056"->OpenRayTracer`createSphericalLens[0,257.09`,-257.09`,2.63`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1056",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1092"->OpenRayTracer`createSphericalLens[0,14.6`,-14.6`,4.7`,12.7`,"N-BK7",OpenRayTracer`PartId->"LB1092",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1106"->OpenRayTracer`createSphericalLens[0,127.37`,-127.37`,8.12`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1106",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1157"->OpenRayTracer`createSphericalLens[0,9.87`,-9.87`,2.43`,6.`,"N-BK7",OpenRayTracer`PartId->"LB1157",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1199"->OpenRayTracer`createSphericalLens[0,204.98`,-204.98`,6.16`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1199",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1212"->OpenRayTracer`createSphericalLens[0,20.11`,-20.11`,2.82`,9.`,"N-BK7",OpenRayTracer`PartId->"LB1212",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1247"->OpenRayTracer`createSphericalLens[0,771.96`,-771.96`,3.84`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1247",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1258"->OpenRayTracer`createSphericalLens[0,30.36`,-30.36`,3.14`,12.7`,"N-BK7",OpenRayTracer`PartId->"LB1258",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1294"->OpenRayTracer`createSphericalLens[0,179.78`,-179.78`,2.9`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1294",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1309"->OpenRayTracer`createSphericalLens[0,75.19`,-75.19`,11.84`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1309",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1374"->OpenRayTracer`createSphericalLens[0,153.28`,-153.28`,7.24`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1374",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1378"->OpenRayTracer`createSphericalLens[0,40.72`,-40.72`,2.8`,12.7`,"N-BK7",OpenRayTracer`PartId->"LB1378",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1391"->OpenRayTracer`createSphericalLens[0,411.65`,-411.65`,2.39`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1391",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1406"->OpenRayTracer`createSphericalLens[0,11.96`,-11.96`,2.26`,6.`,"N-BK7",OpenRayTracer`PartId->"LB1406",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1409"->OpenRayTracer`createSphericalLens[0,1029.79`,-1029.79`,2.16`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1409",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1437"->OpenRayTracer`createSphericalLens[0,154.`,-154.`,3.05`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1437",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1450"->OpenRayTracer`createSphericalLens[0,19.92`,-19.92`,3.88`,12.7`,"N-BK7",OpenRayTracer`PartId->"LB1450",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1471"->OpenRayTracer`createSphericalLens[0,50.6`,-50.6`,5.24`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1471",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1475"->OpenRayTracer`createSphericalLens[0,772.24`,-772.24`,2.21`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1475",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1494"->OpenRayTracer`createSphericalLens[0,11.71`,-11.71`,3.6`,9.`,"N-BK7",OpenRayTracer`PartId->"LB1494",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1596"->OpenRayTracer`createSphericalLens[0,61.`,-61.`,4.67`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1596",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1607"->OpenRayTracer`createSphericalLens[0,179.14`,-179.14`,6.62`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1607",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1630"->OpenRayTracer`createSphericalLens[0,101.38`,-101.38`,9.47`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1630",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1676"->OpenRayTracer`createSphericalLens[0,102.4`,-102.4`,3.58`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1676",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1723"->OpenRayTracer`createSphericalLens[0,59.24`,-59.24`,14.44`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1723",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1757"->OpenRayTracer`createSphericalLens[0,29.52`,-29.52`,7.74`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1757",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1761"->OpenRayTracer`createSphericalLens[0,24.53`,-24.53`,8.98`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1761",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1779"->OpenRayTracer`createSphericalLens[0,308.62`,-308.62`,2.52`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1779",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1811"->OpenRayTracer`createSphericalLens[0,34.86`,-34.86`,6.79`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1811",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1844"->OpenRayTracer`createSphericalLens[0,51.06`,-51.06`,2.59`,12.7`,"N-BK7",OpenRayTracer`PartId->"LB1844",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1859"->OpenRayTracer`createSphericalLens[0,1029.54`,-1029.54`,3.63`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1859",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1862"->OpenRayTracer`createSphericalLens[0,411.28`,-411.28`,4.57`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1862",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1869"->OpenRayTracer`createSphericalLens[0,514.68`,-514.68`,2.31`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1869",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1889"->OpenRayTracer`createSphericalLens[0,256.59`,-256.59`,5.52`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1889",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1901"->OpenRayTracer`createSphericalLens[0,76.55`,-76.55`,4.12`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1901",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1904"->OpenRayTracer`createSphericalLens[0,128.21`,-128.21`,3.26`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1904",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1909"->OpenRayTracer`createSphericalLens[0,514.35`,-514.35`,4.26`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1909",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1917"->OpenRayTracer`createSphericalLens[0,308.17`,-308.17`,5.1`,50.8`,"N-BK7",OpenRayTracer`PartId->"LB1917",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LB1945"->OpenRayTracer`createSphericalLens[0,205.55`,-205.55`,2.79`,25.4`,"N-BK7",OpenRayTracer`PartId->"LB1945",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*data downloaded on 12.06.2020 from the Thorlabs website,Association[Join[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NBK7_PlanoConcave"]]]]*)thorlabsNBK7PlanoConcave=<|"LC1054"->OpenRayTracer`createSphericalLens[0,-12.86`,\[Infinity],3.`,12.7`,"N-BK7",OpenRayTracer`PartId->"LC1054",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1060"->OpenRayTracer`createSphericalLens[0,-15.43`,\[Infinity],3.`,12.7`,"N-BK7",OpenRayTracer`PartId->"LC1060",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1093"->OpenRayTracer`createSphericalLens[0,-51.46`,\[Infinity],4.`,50.8`,"N-BK7",OpenRayTracer`PartId->"LC1093",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1120"->OpenRayTracer`createSphericalLens[0,-51.46`,\[Infinity],4.`,25.4`,"N-BK7",OpenRayTracer`PartId->"LC1120",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1254"->OpenRayTracer`createSphericalLens[0,-51.5`,\[Infinity],4.`,25.`,"N-BK7",OpenRayTracer`PartId->"LC1254",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LC1258"->OpenRayTracer`createSphericalLens[0,-38.6`,\[Infinity],3.5`,25.`,"N-BK7",OpenRayTracer`PartId->"LC1258",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LC1259"->OpenRayTracer`createSphericalLens[0,-25.7`,\[Infinity],3.5`,25.`,"N-BK7",OpenRayTracer`PartId->"LC1259",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LC1315"->OpenRayTracer`createSphericalLens[0,-38.59`,\[Infinity],3.5`,50.8`,"N-BK7",OpenRayTracer`PartId->"LC1315",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1439"->OpenRayTracer`createSphericalLens[0,-25.73`,\[Infinity],3.5`,12.7`,"N-BK7",OpenRayTracer`PartId->"LC1439",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1582"->OpenRayTracer`createSphericalLens[0,-38.59`,\[Infinity],3.5`,25.4`,"N-BK7",OpenRayTracer`PartId->"LC1582",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1611"->OpenRayTracer`createSphericalLens[0,-77.19`,\[Infinity],4.`,50.8`,"N-BK7",OpenRayTracer`PartId->"LC1611",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1715"->OpenRayTracer`createSphericalLens[0,-25.73`,\[Infinity],3.5`,25.4`,"N-BK7",OpenRayTracer`PartId->"LC1715",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1906"->OpenRayTracer`createSphericalLens[0,-13.89`,\[Infinity],2,9.`,"N-BK7",OpenRayTracer`PartId->"LC1906",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LC1975"->OpenRayTracer`createSphericalLens[0,-12.35`,\[Infinity],2,6.`,"N-BK7",OpenRayTracer`PartId->"LC1975",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*data downloaded on 12.06.2020,two entries addedmanually due to lack of zemax files,Association[Join[Map[readThorlabsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NBK7_Plano_Convex"]],Map[readThorlabsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\NBK7_Plano_Convex\\Unicode"]]]]*)thorlabsNBK7PlanoConvex=<|(*added manually*)"LA1509"->OpenRayTracer`createSphericalLens[0,51.5,\[Infinity],3.6,25.4,"N-BK7",OpenRayTracer`PartId->"LA1509",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1119"->OpenRayTracer`createSphericalLens[0,25.8,\[Infinity],3.4,18.0,"N-BK7",OpenRayTracer`PartId->"LA1119",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1002"->OpenRayTracer`createSphericalLens[0,77.26`,\[Infinity],12.71`,75.`,"N-BK7",OpenRayTracer`PartId->"LA1002",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1027"->OpenRayTracer`createSphericalLens[0,18.02`,\[Infinity],7.23`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1027",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1031"->OpenRayTracer`createSphericalLens[0,51.5`,\[Infinity],4.73`,30.`,"N-BK7",OpenRayTracer`PartId->"LA1031",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1050"->OpenRayTracer`createSphericalLens[0,51.5`,\[Infinity],9.69`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1050",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1074"->OpenRayTracer`createSphericalLens[0,10.3`,\[Infinity],3.98`,12.7`,"N-BK7",OpenRayTracer`PartId->"LA1074",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1085"->OpenRayTracer`createSphericalLens[0,15.45`,\[Infinity],4.69`,18.`,"N-BK7",OpenRayTracer`PartId->"LA1085",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1102"->OpenRayTracer`createSphericalLens[0,25.75`,\[Infinity],7.31`,30.`,"N-BK7",OpenRayTracer`PartId->"LA1102",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1116"->OpenRayTracer`createSphericalLens[0,5.15`,\[Infinity],2.46`,6.`,"N-BK7",OpenRayTracer`PartId->"LA1116",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1131"->OpenRayTracer`createSphericalLens[0,25.75`,\[Infinity],5.34`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1131",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1134"->OpenRayTracer`createSphericalLens[0,30.9`,\[Infinity],4.73`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1134",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1145"->OpenRayTracer`createSphericalLens[0,38.63`,\[Infinity],12.52`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1145",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1172"->OpenRayTracer`createSphericalLens[0,206.03`,\[Infinity],2.39`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1172",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1207"->OpenRayTracer`createSphericalLens[0,51.5`,\[Infinity],2.19`,12.7`,"N-BK7",OpenRayTracer`PartId->"LA1207",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1213"->OpenRayTracer`createSphericalLens[0,25.75`,\[Infinity],2.59`,12.7`,"N-BK7",OpenRayTracer`PartId->"LA1213",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1222"->OpenRayTracer`createSphericalLens[0,7.72`,\[Infinity],2.1`,6.`,"N-BK7",OpenRayTracer`PartId->"LA1222",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1229"->OpenRayTracer`createSphericalLens[0,90.13`,\[Infinity],2.89`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1229",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1237"->OpenRayTracer`createSphericalLens[0,257.54`,\[Infinity],2.31`,30.`,"N-BK7",OpenRayTracer`PartId->"LA1237",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1238"->OpenRayTracer`createSphericalLens[0,51.5`,\[Infinity],19.19`,75.`,"N-BK7",OpenRayTracer`PartId->"LA1238",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1251"->OpenRayTracer`createSphericalLens[0,51.5`,\[Infinity],3.6`,25.`,"N-BK7",OpenRayTracer`PartId->"LA1251",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1252"->OpenRayTracer`createSphericalLens[0,13.1`,\[Infinity],11.7`,25.`,"N-BK7",OpenRayTracer`PartId->"LA1252",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1253"->OpenRayTracer`createSphericalLens[0,103.`,\[Infinity],2.8`,25.`,"N-BK7",OpenRayTracer`PartId->"LA1253",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1255"->OpenRayTracer`createSphericalLens[0,25.8`,\[Infinity],5.3`,25.`,"N-BK7",OpenRayTracer`PartId->"LA1255",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1256"->OpenRayTracer`createSphericalLens[0,154.52`,\[Infinity],5.1`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1256",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1257"->OpenRayTracer`createSphericalLens[0,38.6`,\[Infinity],4.1`,25.`,"N-BK7",OpenRayTracer`PartId->"LA1257",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1270"->OpenRayTracer`createSphericalLens[0,12.87`,\[Infinity],5.46`,18.`,"N-BK7",OpenRayTracer`PartId->"LA1270",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1274"->OpenRayTracer`createSphericalLens[0,20.6`,\[Infinity],8.97`,30.`,"N-BK7",OpenRayTracer`PartId->"LA1274",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1289"->OpenRayTracer`createSphericalLens[0,15.45`,\[Infinity],3.16`,12.7`,"N-BK7",OpenRayTracer`PartId->"LA1289",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1301"->OpenRayTracer`createSphericalLens[0,128.77`,\[Infinity],5.52`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1301",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1304"->OpenRayTracer`createSphericalLens[0,20.6`,\[Infinity],2.8`,12.7`,"N-BK7",OpenRayTracer`PartId->"LA1304",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1353"->OpenRayTracer`createSphericalLens[0,103.01`,\[Infinity],10.06`,75.`,"N-BK7",OpenRayTracer`PartId->"LA1353",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1380"->OpenRayTracer`createSphericalLens[0,257.54`,\[Infinity],4.25`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1380",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1384"->OpenRayTracer`createSphericalLens[0,64.38`,\[Infinity],8.22`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1384",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1399"->OpenRayTracer`createSphericalLens[0,90.13`,\[Infinity],6.65`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1399",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1401"->OpenRayTracer`createSphericalLens[0,30.9`,\[Infinity],16.29`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1401",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1417"->OpenRayTracer`createSphericalLens[0,77.26`,\[Infinity],7.29`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1417",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1419"->OpenRayTracer`createSphericalLens[0,154.52`,\[Infinity],2.52`,30.`,"N-BK7",OpenRayTracer`PartId->"LA1419",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1433"->OpenRayTracer`createSphericalLens[0,77.26`,\[Infinity],3.05`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1433",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1461"->OpenRayTracer`createSphericalLens[0,128.77`,\[Infinity],2.62`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1461",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1464"->OpenRayTracer`createSphericalLens[0,515.08`,\[Infinity],2.15`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1464",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1470"->OpenRayTracer`createSphericalLens[0,6.18`,\[Infinity],2.27`,6.`,"N-BK7",OpenRayTracer`PartId->"LA1470",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1472"->OpenRayTracer`createSphericalLens[0,10.3`,\[Infinity],2.53`,9.`,"N-BK7",OpenRayTracer`PartId->"LA1472",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1484"->OpenRayTracer`createSphericalLens[0,154.52`,\[Infinity],2.52`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1484",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1540"->OpenRayTracer`createSphericalLens[0,7.72`,\[Infinity],5.12`,12.7`,"N-BK7",OpenRayTracer`PartId->"LA1540",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1541"->OpenRayTracer`createSphericalLens[0,103.01`,\[Infinity],2.78`,30.`,"N-BK7",OpenRayTracer`PartId->"LA1541",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1560"->OpenRayTracer`createSphericalLens[0,12.87`,\[Infinity],3.47`,12.7`,"N-BK7",OpenRayTracer`PartId->"LA1560",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1576"->OpenRayTracer`createSphericalLens[0,6.18`,\[Infinity],3.44`,9.`,"N-BK7",OpenRayTracer`PartId->"LA1576",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1608"->OpenRayTracer`createSphericalLens[0,38.63`,\[Infinity],4.14`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1608",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1700"->OpenRayTracer`createSphericalLens[0,15.45`,\[Infinity],1.79`,6.`,"N-BK7",OpenRayTracer`PartId->"LA1700",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1708"->OpenRayTracer`createSphericalLens[0,103.01`,\[Infinity],2.78`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1708",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1725"->OpenRayTracer`createSphericalLens[0,206.03`,\[Infinity],4.57`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1725",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1727"->OpenRayTracer`createSphericalLens[0,386.31`,\[Infinity],3.83`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1727",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1740"->OpenRayTracer`createSphericalLens[0,43.78`,\[Infinity],24.18`,75.`,"N-BK7",OpenRayTracer`PartId->"LA1740",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1765"->OpenRayTracer`createSphericalLens[0,38.63`,\[Infinity],5.53`,30.`,"N-BK7",OpenRayTracer`PartId->"LA1765",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1779"->OpenRayTracer`createSphericalLens[0,515.08`,\[Infinity],3.62`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1779",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1805"->OpenRayTracer`createSphericalLens[0,15.45`,\[Infinity],8.64`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1805",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1832"->OpenRayTracer`createSphericalLens[0,128.77`,\[Infinity],2.62`,30.`,"N-BK7",OpenRayTracer`PartId->"LA1832",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1859"->OpenRayTracer`createSphericalLens[0,10.3`,\[Infinity],7.08`,18.`,"N-BK7",OpenRayTracer`PartId->"LA1859",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1907"->OpenRayTracer`createSphericalLens[0,77.26`,\[Infinity],3.05`,30.`,"N-BK7",OpenRayTracer`PartId->"LA1907",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1908"->OpenRayTracer`createSphericalLens[0,257.54`,\[Infinity],2.31`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1908",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1951"->OpenRayTracer`createSphericalLens[0,13.08`,\[Infinity],11.74`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1951",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1978"->OpenRayTracer`createSphericalLens[0,386.31`,\[Infinity],2.2`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1978",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1979"->OpenRayTracer`createSphericalLens[0,103.01`,\[Infinity],6.18`,50.8`,"N-BK7",OpenRayTracer`PartId->"LA1979",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1986"->OpenRayTracer`createSphericalLens[0,64.38`,\[Infinity],3.26`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1986",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"LA1024"->OpenRayTracer`createSphericalLens[0,2.0672268907563027`,\[Infinity],1,2,"N-BK7",OpenRayTracer`PartId->"LA1024",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1026"->OpenRayTracer`createSphericalLens[0,3.10077519379845`,\[Infinity],1,2,"N-BK7",OpenRayTracer`PartId->"LA1026",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1036"->OpenRayTracer`createSphericalLens[0,3.10077519379845`,\[Infinity],1.5`,3.`,"N-BK7",OpenRayTracer`PartId->"LA1036",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1039"->OpenRayTracer`createSphericalLens[0,4.650943396226415`,\[Infinity],1.5`,3.`,"N-BK7",OpenRayTracer`PartId->"LA1039",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1254"->OpenRayTracer`createSphericalLens[0,775.2`,\[Infinity],2.1`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1254",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1258"->OpenRayTracer`createSphericalLens[0,1033.6`,\[Infinity],2.1`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1258",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"LA1259"->OpenRayTracer`createSphericalLens[0,1292.`,\[Infinity],2.1`,25.4`,"N-BK7",OpenRayTracer`PartId->"LA1259",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}]|>;


(*zmx files downloaded on 18.02.2020 from EdmundOptics website,Association[Join[Map[readThorlabsZemaxCementedDoubletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\AchromaticCementedDoublets400To700nm"]],Map[readThorlabsZemaxCementedDoubletSphericalLensFile[#,CharacterEncoding->"Unicode"]&,
FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\AchromaticCementedDoublets400To700nm\\Unicode"]]]]*)
thorlabsAchromaticCementedDoublets400To700nm=<|"AC050008A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "5.25`", ",", 
RowBox[{"-", "3.9`"}], ",", 
RowBox[{"-", "17.06`"}], ",", "2.8`", ",", "1.7`", ",", "5.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC050008A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC050010A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "6.55`", ",", 
RowBox[{"-", "4.25`"}], ",", 
RowBox[{"-", "15.42`"}], ",", "2.5`", ",", "1.9`", ",", "5.`", ",", "\"\<N-BAK4\>\"", ",", "\"\<SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC050010A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC050015A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.54`", ",", 
RowBox[{"-", "5.25`"}], ",", 
RowBox[{"-", "12.05`"}], ",", "2.7`", ",", "2.1`", ",", "5.`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC050015A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC060010A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "6.17`", ",", 
RowBox[{"-", "4.61`"}], ",", 
RowBox[{"-", "19.63`"}], ",", "2.5`", ",", "1.5`", ",", "6.`", ",", "\"\<N-BAK4\>\"", ",", "\"\<SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC060010A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC064013A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.08`", ",", 
RowBox[{"-", "5.94`"}], ",", 
RowBox[{"-", "40.36`"}], ",", "2.5`", ",", "1.5`", ",", "6.35`", ",", "\"\<N-BAK4\>\"", ",", "\"\<SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC064013A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC064015A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "8.79`", ",", 
RowBox[{"-", "6.55`"}], ",", 
RowBox[{"-", "21.68`"}], ",", "2.5`", ",", "1.5`", ",", "6.35`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC064015A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080010A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.08`", ",", 
RowBox[{"-", "5.25`"}], ",", 
RowBox[{"-", "22.66`"}], ",", "4.5`", ",", "2", ",", "8.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080010A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080016A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "11.02`", ",", 
RowBox[{"-", "9.2`"}], ",", 
RowBox[{"-", "46.77`"}], ",", "2.5`", ",", "1.5`", ",", "8.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080016A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080020A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "11.08`", ",", 
RowBox[{"-", "9.2`"}], ",", 
RowBox[{"-", "34.83`"}], ",", "2.5`", ",", "1.5`", ",", "8.`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080020A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127019A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.94`", ",", 
RowBox[{"-", "11.04`"}], ",", 
RowBox[{"-", "59.26`"}], ",", "4.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127019A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127025A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "18.79`", ",", 
RowBox[{"-", "10.59`"}], ",", 
RowBox[{"-", "68.08`"}], ",", "5.`", ",", "2", ",", "12.7`", ",", "\"\<N-BAF10\>\"", ",", "\"\<SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127025A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\"", ",", "\"\<HIKARI\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127030A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "17.865079365079364`", ",", 
RowBox[{"-", "13.53`"}], ",", 
RowBox[{"-", "44.17`"}], ",", "3.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127030A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127050A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "27.36`", ",", 
RowBox[{"-", "22.54`"}], ",", 
RowBox[{"-", "91.83`"}], ",", "3.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127050A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127075A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "41.3`", ",", 
RowBox[{"-", "33.96`"}], ",", 
RowBox[{"-", "137.09`"}], ",", "2.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127075A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254030A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "20.89`", ",", 
RowBox[{"-", "16.73`"}], ",", 
RowBox[{"-", "79.8`"}], ",", "12.`", ",", "2", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254030A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254035A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "23.99`", ",", 
RowBox[{"-", "19.1`"}], ",", 
RowBox[{"-", "102.09`"}], ",", "12.`", ",", "2", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254035A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254040A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "23.66`", ",", 
RowBox[{"-", "20.09`"}], ",", 
RowBox[{"-", "57.68`"}], ",", "10.`", ",", "2.5`", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254040A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254045A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "31.24`", ",", 
RowBox[{"-", "25.94`"}], ",", 
RowBox[{"-", "130.62`"}], ",", "7.`", ",", "2", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254045A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254050A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "33.34`", ",", 
RowBox[{"-", "22.28`"}], ",", 
RowBox[{"-", "291.07`"}], ",", "9.`", ",", "2.5`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254050A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\"", ",", "\"\<HIKARI\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254060A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "41.69`", ",", 
RowBox[{"-", "25.88`"}], ",", 
RowBox[{"-", "230.7`"}], ",", "8.`", ",", "2.5`", ",", "25.4`", ",", "\"\<E-BAF11\>\"", ",", "\"\<FD10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254060A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\"", ",", "\"\<HIKARI\>\"", ",", "\"\<HOYA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254075A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "46.54`", ",", 
RowBox[{"-", "33.91`"}], ",", 
RowBox[{"-", "95.54`"}], ",", "7.`", ",", "2.5`", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254075A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254080A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "49.58490566037736`", ",", 
RowBox[{"-", "35.535714285714285`"}], ",", 
RowBox[{"-", "101.1948051948052`"}], ",", "7.00048775857`", ",", "2.999769679963`", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254080A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254100A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "62.75`", ",", 
RowBox[{"-", "45.71`"}], ",", 
RowBox[{"-", "128.23`"}], ",", "4.`", ",", "2.5`", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254100A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254125A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "77.62987012987013`", ",", 
RowBox[{"-", "55.92035398230089`"}], ",", 
RowBox[{"-", "160.82191780821918`"}], ",", "4.003571813499`", ",", "2.833474807849`", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254125A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254150A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "91.62`", ",", 
RowBox[{"-", "66.68`"}], ",", 
RowBox[{"-", "197.7`"}], ",", "5.7`", ",", "2.2`", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254150A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254200A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "77.4`", ",", 
RowBox[{"-", "87.57`"}], ",", "291.07`", ",", "4.`", ",", "2.5`", ",", "25.4`", ",", "\"\<N-SSK5\>\"", ",", "\"\<LAFN7\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254200A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254250A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "137.09`", ",", 
RowBox[{"-", "111.51`"}], ",", 
RowBox[{"-", "459.2`"}], ",", "4.`", ",", "2", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254250A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254300A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "165.2`", ",", 
RowBox[{"-", "137.09`"}], ",", 
RowBox[{"-", "557.4`"}], ",", "4.`", ",", "2", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254300A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254400A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "219.8`", ",", 
RowBox[{"-", "181.55`"}], ",", 
RowBox[{"-", "738.5`"}], ",", "4.`", ",", "2", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254400A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254500A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "337.3`", ",", 
RowBox[{"-", "186.75`"}], ",", 
RowBox[{"-", "557.4`"}], ",", "4.`", ",", "2", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254500A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC300050A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "34.04`", ",", 
RowBox[{"-", "29.38`"}], ",", 
RowBox[{"-", "161.5`"}], ",", "8.5`", ",", "2", ",", "30.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC300050A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC300080A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "55.98`", ",", 
RowBox[{"-", "44.17`"}], ",", 
RowBox[{"-", "219.8`"}], ",", "8.5`", ",", "2", ",", "30.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC300080A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC300100A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "70.`", ",", 
RowBox[{"-", "57.02`"}], ",", 
RowBox[{"-", "284.4`"}], ",", "5.`", ",", "2", ",", "30.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC300100A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508075A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "50.8`", ",", 
RowBox[{"-", "41.69`"}], ",", 
RowBox[{"-", "247.7`"}], ",", "20.`", ",", "3.`", ",", "50.8`", ",", "\"\<E-BAF11\>\"", ",", "\"\<N-SF11\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508075A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\"", ",", "\"\<HIKARI\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508080A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "54.9`", ",", 
RowBox[{"-", "46.4`"}], ",", 
RowBox[{"-", "247.2`"}], ",", "16.`", ",", "2", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508080A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC5081000A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "738.5`", ",", 
RowBox[{"-", "398.1`"}], ",", 
RowBox[{"-", "1023.3`"}], ",", "4.`", ",", "2", ",", "50.8`", ",", "\"\<N-BK7\>\"", ",", "\"\<SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC5081000A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508100A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "71.12`", ",", 
RowBox[{"-", "44.17`"}], ",", 
RowBox[{"-", "363.1`"}], ",", "16.`", ",", "4.`", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508100A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\"", ",", "\"\<HIKARI\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080030A"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "15.95`", ",", 
RowBox[{"-", "13.53`"}], ",", 
RowBox[{"-", "59.43`"}], ",", "2.5`", ",", "1.5`", ",", "8", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080030A\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\)|>;


(*zmx files downloaded on 18.02.2020 from EdmundOptics website,
Association[Map[readThorlabsZemaxCementedDoubletSphericalLensFile[#,CharacterEncoding->"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\AchromaticCementedDoublets400To1100nm"]]]*)
thorlabsAchromaticCementedDoublets400To1100nm=<|"AC127019AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "14.287878787878787`", ",", 
RowBox[{"-", "13.762376237623762`"}], ",", 
RowBox[{"-", "68.51587301587301`"}], ",", "4", ",", "0.9998024122369`", ",", "12.7`", ",", "\"\<N-LAK10\>\"", ",", "\"\<N-SF57\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127019AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"AC127025AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "19.119266055045873`", ",", 
RowBox[{"-", "17.821656050955415`"}], ",", 
RowBox[{"-", "82.3211009174312`"}], ",", "4.000100905691`", ",", "2", ",", "12.7`", ",", "\"\<N-LAK10\>\"", ",", "\"\<N-SF57\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127025AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127030AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "16.748917748917748`", ",", 
RowBox[{"-", "13.822916666666666`"}], ",", 
RowBox[{"-", "52.14`"}], ",", "4.000053094432`", ",", "1", ",", "12.7`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127030AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127050AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "26.335`", ",", 
RowBox[{"-", "22.63855421686747`"}], ",", 
RowBox[{"-", "102.43859649122807`"}], ",", "4", ",", "2", ",", "12.7`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127050AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127075AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "38.36434108527132`", ",", 
RowBox[{"-", "34.962025316455694`"}], ",", 
RowBox[{"-", "177.29090909090908`"}], ",", "2.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF2\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127075AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254030AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "20.027397260273972`", ",", 
RowBox[{"-", "17.44`"}], ",", 
RowBox[{"-", "93.08181818181818`"}], ",", "12", ",", "3", ",", "25.4`", ",", "\"\<S-BAH11\>\"", ",", "\"\<S-TIH6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254030AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254050AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "34.93`", ",", 
RowBox[{"-", "28.76595744680851`"}], ",", 
RowBox[{"-", "137.5344827586207`"}], ",", "9", ",", "3.5`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254050AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254075AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "51.982608695652175`", ",", 
RowBox[{"-", "43.36329588014981`"}], ",", 
RowBox[{"-", "217.36231884057972`"}], ",", "8", ",", "4", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254075AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254100AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "92.3972602739726`", ",", 
RowBox[{"-", "48.1764705882353`"}], ",", 
RowBox[{"-", "152.81944444444446`"}], ",", "8", ",", "4", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254100AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254150AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "87.86086956521739`", ",", 
RowBox[{"-", "105.64444444444445`"}], ",", "\[Infinity]", ",", "6.000000801374`", ",", "3", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254150AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254200AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "117.12345679012346`", ",", 
RowBox[{"-", "142.11811023622047`"}], ",", "\[Infinity]", ",", "5", ",", "3", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254200AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508080AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "63.62105263157895`", ",", 
RowBox[{"-", "80.62142857142857`"}], ",", 
RowBox[{"-", "181.7017543859649`"}], ",", "12", ",", "3", ",", "50.8`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508080AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"AC508180AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "144.44444444444446`", ",", 
RowBox[{"-", "115.428`"}], ",", 
RowBox[{"-", "328.2012195121951`"}], ",", "9.5`", ",", "4", ",", "50.8`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508180AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508300AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "167.7153846153846`", ",", 
RowBox[{"-", "285.81538461538463`"}], ",", "\[Infinity]", ",", "9", ",", "4", ",", "50.8`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508300AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<CDGM\>\"", ",", "\"\<SCHOTT\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508400AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "184.25174825174824`", ",", 
RowBox[{"-", "274.02222222222224`"}], ",", "\[Infinity]", ",", "9", ",", "4", ",", "50.8`", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508400AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<CDGM\>\"", ",", "\"\<LOWSTAIN\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508500AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "230.3361344537815`", ",", 
RowBox[{"-", "343.93506493506493`"}], ",", "\[Infinity]", ",", "9", ",", "4", ",", "50.8`", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508500AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<OHARA\>\"", ",", "\"\<SCHOTT\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508600AB"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "276.42076502732243`", ",", 
RowBox[{"-", "413.85207100591714`"}], ",", "\[Infinity]", ",", "9", ",", "4", ",", "50.8`", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508600AB\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<OHARA\>\"", ",", "\"\<SCHOTT\>\""}], "}"}]}]}], "]"}],
HoldForm]\)|>;


(*zmx files downloaded on 17.02.2020 from EdmundOptics website,Association[Join[Map[readThorlabsZemaxCementedDoubletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\AchromaticCementedDoublets650To1050nm"]],
Map[readThorlabsZemaxCementedDoubletSphericalLensFile[#,CharacterEncoding->"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\AchromaticCementedDoublets650To1050nm\\Unicode"]]]]*)
thorlabsAchromaticCementedDoublets650To1050nm =<|"AC050008B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "4.61`", ",", 
RowBox[{"-", "3.9`"}], ",", 
RowBox[{"-", "35.95`"}], ",", "2.8`", ",", "1.8`", ",", "5.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC050008B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC050010B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "6.55`", ",", 
RowBox[{"-", "5.25`"}], ",", 
RowBox[{"-", "24.89`"}], ",", "2.2`", ",", "1.6`", ",", "5.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC050010B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC050015B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "10.28`", ",", 
RowBox[{"-", "7.59`"}], ",", 
RowBox[{"-", "32.14`"}], ",", "2.3`", ",", "1.7`", ",", "5.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC050015B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC060010B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.08`", ",", 
RowBox[{"-", "5.25`"}], ",", 
RowBox[{"-", "19.54`"}], ",", "2.5`", ",", "1.5`", ",", "6.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC060010B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC064013B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "8.6`", ",", 
RowBox[{"-", "6.67`"}], ",", 
RowBox[{"-", "28.97`"}], ",", "2.5`", ",", "1.4`", ",", "6.35`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC064013B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC064015B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "10.28`", ",", 
RowBox[{"-", "7.75`"}], ",", 
RowBox[{"-", "32.89`"}], ",", "2.4`", ",", "1.5`", ",", "6.35`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC064015B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080010B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.55`", ",", 
RowBox[{"-", "4.61`"}], ",", 
RowBox[{"-", "30.62`"}], ",", "4.5`", ",", "1.3`", ",", "8.`", ",", "\"\<N-LAK10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080010B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080016B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "11.02`", ",", 
RowBox[{"-", "8.6`"}], ",", 
RowBox[{"-", "35.81`"}], ",", "2.5`", ",", "1.5`", ",", "8.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080016B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080020B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "13.53`", ",", 
RowBox[{"-", "10.59`"}], ",", 
RowBox[{"-", "47.75`"}], ",", "2.3`", ",", "1.3`", ",", "8.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080020B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127019B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.16`", ",", 
RowBox[{"-", "10.59`"}], ",", 
RowBox[{"-", "77.4`"}], ",", "4.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127019B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127025B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "16.18`", ",", 
RowBox[{"-", "13.31`"}], ",", 
RowBox[{"-", "68.54`"}], ",", "5.`", ",", "2", ",", "12.7`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127025B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127030B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "19.82`", ",", 
RowBox[{"-", "16.18`"}], ",", 
RowBox[{"-", "79.08`"}], ",", "3.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127030B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127050B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "24.2`", ",", 
RowBox[{"-", "26.79`"}], ",", "250.`", ",", "3.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127050B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127075B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "36.22`", ",", 
RowBox[{"-", "40.36`"}], ",", "398.1`", ",", "2.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127075B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254030B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "21.09`", ",", 
RowBox[{"-", "16.18`"}], ",", 
RowBox[{"-", "79.08`"}], ",", "12.`", ",", "1.5`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254030B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254035B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "23.99`", ",", 
RowBox[{"-", "18.62`"}], ",", 
RowBox[{"-", "97.27`"}], ",", "10.5`", ",", "1.5`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254035B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254040B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "26.12`", ",", 
RowBox[{"-", "21.28`"}], ",", 
RowBox[{"-", "137.09`"}], ",", "10.`", ",", "2.5`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254040B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254045B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "29.38`", ",", 
RowBox[{"-", "25.05`"}], ",", 
RowBox[{"-", "127.06`"}], ",", "7.8`", ",", "1.6`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254045B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254050B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "33.55`", ",", 
RowBox[{"-", "27.05`"}], ",", 
RowBox[{"-", "125.6`"}], ",", "7.5`", ",", "1.8`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254050B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254060B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "39.48`", ",", 
RowBox[{"-", "33.`"}], ",", 
RowBox[{"-", "165.2`"}], ",", "6.`", ",", "1.7`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254060B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254075B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "36.9`", ",", 
RowBox[{"-", "42.17`"}], ",", "417.8`", ",", "5.`", ",", "1.6`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254075B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254080B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "38.66990291262136`", ",", 
RowBox[{"-", "43.20388349514563`"}], ",", "373.969696969697`", ",", "6.582144861689`", ",", "2.027306446738`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254080B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254100B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "66.68`", ",", 
RowBox[{"-", "53.7`"}], ",", 
RowBox[{"-", "259.41`"}], ",", "4.`", ",", "1.5`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254100B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254150B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "83.6`", ",", 
RowBox[{"-", "89.33`"}], ",", 
RowBox[{"-", "1330.5`"}], ",", "4.`", ",", "3.5`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254150B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254200B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "106.41`", ",", 
RowBox[{"-", "96.61`"}], ",", "2000.`", ",", "4.`", ",", "4.`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254200B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254250B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "52.`", ",", 
RowBox[{"-", "65.31`"}], ",", "111.51`", ",", "4.`", ",", "1.5`", ",", "25.4`", ",", "\"\<SF5\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254250B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254300B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "62.4`", ",", 
RowBox[{"-", "77.4`"}], ",", "134.`", ",", "4.`", ",", "2", ",", "25.4`", ",", "\"\<SF5\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254300B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254400B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "83.6`", ",", 
RowBox[{"-", "106.41`"}], ",", "181.55`", ",", "3.5`", ",", "1.8`", ",", "25.4`", ",", "\"\<SF5\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254400B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254500B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "60.6`", ",", 
RowBox[{"-", "62.75`"}], ",", "87.57`", ",", "4.`", ",", "2", ",", "25.4`", ",", "\"\<SF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254500B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC300050B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "30.76`", ",", 
RowBox[{"-", "27.86`"}], ",", 
RowBox[{"-", "272.9`"}], ",", "9.5`", ",", "2", ",", "30.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC300050B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC300080B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "52.48`", ",", 
RowBox[{"-", "42.69`"}], ",", 
RowBox[{"-", "216.3`"}], ",", "6.5`", ",", "2", ",", "30.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC300080B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC300100B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "49.09`", ",", 
RowBox[{"-", "55.34`"}], ",", "557.4`", ",", "6.`", ",", "2", ",", "30.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC300100B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508075B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "51.8`", ",", 
RowBox[{"-", "93.11`"}], ",", 
RowBox[{"-", "291.07`"}], ",", "12.`", ",", "5.`", ",", "50.8`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508075B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508080B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "51.8`", ",", 
RowBox[{"-", "44.6`"}], ",", 
RowBox[{"-", "312.6`"}], ",", "16.`", ",", "2", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508080B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC5081000B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "494.3`", ",", 
RowBox[{"-", "398.1`"}], ",", "3440.`", ",", "4.2`", ",", "2.8`", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC5081000B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508100B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "65.77`", ",", 
RowBox[{"-", "55.98`"}], ",", 
RowBox[{"-", "280.55`"}], ",", "13.`", ",", "2", ",", "50.8`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508100B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508150B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "112.21`", ",", 
RowBox[{"-", "95.94`"}], ",", 
RowBox[{"-", "325.1`"}], ",", "8.2`", ",", "5.`", ",", "50.8`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508150B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508200B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "134.`", ",", 
RowBox[{"-", "109.2`"}], ",", 
RowBox[{"-", "515.2`"}], ",", "8.2`", ",", "5.`", ",", "50.8`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508200B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080030B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "18.48`", ",", 
RowBox[{"-", "16.18`"}], ",", 
RowBox[{"-", "105.98`"}], ",", "2.5`", ",", "1.5`", ",", "8", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080030B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254125B"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "44.45238095238095`", ",", 
RowBox[{"-", "55.266129032258064`"}], ",", "930.4714285714285`", ",", "5.999999397069`", ",", "6.000040431431`", ",", "25.4`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF8\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254125B\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\)|>;


(*zmx files downloaded on 18.02.2020 from EdmundOptics website,one entry added manually based on a pdf drawing,Association[Join[Map[readThorlabsZemaxCementedDoubletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\AchromaticCementedDoublets650to1700nm"]],Map[readThorlabsZemaxCementedDoubletSphericalLensFile[#,CharacterEncoding->"Unicode"]&,
FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\Thorlabs\\AchromaticCementedDoublets650to1700nm\\Unicode"]]]]*)
thorlabsAchromaticCementedDoublets650To1700nm=<|
(*entry added manually*)"AC060010C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "10.4", ",", 
RowBox[{"-", "3.6"}], ",", 
RowBox[{"-", "9.2"}], ",", "3.5`", ",", "1.3`", ",", "6", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC060010C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\)

"AC050008C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "4.61`", ",", 
RowBox[{"-", "3.9`"}], ",", 
RowBox[{"-", "23.88`"}], ",", "2.5`", ",", "1.5`", ",", "5.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC050008C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC050010C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "4.61`", ",", 
RowBox[{"-", "4.61`"}], ",", "35.95`", ",", "2.5`", ",", "1.5`", ",", "5.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC050010C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC050015C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "5.25`", ",", 
RowBox[{"-", "5.45`"}], ",", "15.24`", ",", "2", ",", "1.3`", ",", "5.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC050015C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC064013C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "13.15`", ",", 
RowBox[{"-", "4.89`"}], ",", 
RowBox[{"-", "12.42`"}], ",", "2.8`", ",", "1.3`", ",", "6.35`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC064013C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC064015C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "22.7`", ",", 
RowBox[{"-", "4.89`"}], ",", 
RowBox[{"-", "11.32`"}], ",", "2.3`", ",", "1.3`", ",", "6.35`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC064015C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080010C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.08`", ",", 
RowBox[{"-", "4.89`"}], ",", 
RowBox[{"-", "20.89`"}], ",", "4.2`", ",", "1.3`", ",", "8.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080010C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080016C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.45`", ",", 
RowBox[{"-", "7.75`"}], ",", "68.54`", ",", "3.5`", ",", "1.3`", ",", "8.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080016C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080020C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.75`", ",", 
RowBox[{"-", "8.6`"}], ",", "31.92`", ",", "3.3`", ",", "1.3`", ",", "8.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080020C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127019C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.42`", ",", 
RowBox[{"-", "10.01`"}], ",", 
RowBox[{"-", "48.75`"}], ",", "5.`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127019C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127025C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.`", ",", 
RowBox[{"-", "12.94`"}], ",", "151.71`", ",", "4.7`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127025C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127030C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.42`", ",", 
RowBox[{"-", "14.03`"}], ",", "65.31`", ",", "4.7`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127030C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127050C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "15.95`", ",", 
RowBox[{"-", "18.41`"}], ",", "44.6`", ",", "4.`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127050C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC127075C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "23.23`", ",", 
RowBox[{"-", "27.86`"}], ",", "66.68`", ",", "3.`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC127075C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254030C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "21.09`", ",", 
RowBox[{"-", "15.24`"}], ",", 
RowBox[{"-", "71.12`"}], ",", "13.`", ",", "1.8`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254030C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254035C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "23.15`", ",", 
RowBox[{"-", "17.87`"}], ",", 
RowBox[{"-", "105.2`"}], ",", "11.5`", ",", "1.8`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254035C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254040C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "24.43`", ",", 
RowBox[{"-", "21.09`"}], ",", 
RowBox[{"-", "143.92`"}], ",", "10.`", ",", "1.8`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254040C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254045C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "22.94`", ",", 
RowBox[{"-", "23.66`"}], ",", "900.`", ",", "9.6`", ",", "1.8`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254045C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254050C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "22.94`", ",", 
RowBox[{"-", "25.88`"}], ",", "194.54`", ",", "9.`", ",", "1.8`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254050C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254060C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "23.88`", ",", 
RowBox[{"-", "28.12`"}], ",", "112.08`", ",", "8.3`", ",", "1.8`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254060C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254075C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "26.36`", ",", 
RowBox[{"-", "29.38`"}], ",", "84.92`", ",", "7.6`", ",", "1.8`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254075C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254100C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "32.14`", ",", 
RowBox[{"-", "38.02`"}], ",", "93.54`", ",", "6.5`", ",", "1.8`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254100C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254150C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "42.69`", ",", 
RowBox[{"-", "52.`"}], ",", "111.51`", ",", "5.`", ",", "2.5`", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254150C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254200C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "70.`", ",", 
RowBox[{"-", "95.94`"}], ",", "274.31`", ",", "4.`", ",", "3.`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254200C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254250C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "43.95`", ",", 
RowBox[{"-", "57.68`"}], ",", "93.11`", ",", "4.5`", ",", "2.5`", ",", "25.4`", ",", "\"\<SF2\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254250C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254300C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "52.48`", ",", 
RowBox[{"-", "68.54`"}], ",", "112.21`", ",", "4.5`", ",", "2.5`", ",", "25.4`", ",", "\"\<SF2\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254300C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254400C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "70.`", ",", 
RowBox[{"-", "93.11`"}], ",", "151.36`", ",", "4.2`", ",", "2.5`", ",", "25.4`", ",", "\"\<SF2\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254400C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254500C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "87.9`", ",", 
RowBox[{"-", "115.45`"}], ",", "194.54`", ",", "3.5`", ",", "2", ",", "25.4`", ",", "\"\<SF2\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254500C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC300050C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "41.69`", ",", 
RowBox[{"-", "22.7`"}], ",", 
RowBox[{"-", "75.68`"}], ",", "10.`", ",", "2", ",", "30.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC300050C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC300080C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "29.38`", ",", 
RowBox[{"-", "33.91`"}], ",", "97.72`", ",", "9.5`", ",", "2", ",", "30.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC300080C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC300100C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "33.55`", ",", 
RowBox[{"-", "39.17`"}], ",", "100.69`", ",", "8.5`", ",", "2.2`", ",", "30.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC300100C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508075C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "49.89`", ",", 
RowBox[{"-", "39.09`"}], ",", 
RowBox[{"-", "230.7`"}], ",", "19.`", ",", "2.5`", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508075C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508080C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "47.21`", ",", 
RowBox[{"-", "43.15`"}], ",", 
RowBox[{"-", "640.7`"}], ",", "18.`", ",", "2.5`", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508080C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC5081000C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "172.98`", ",", 
RowBox[{"-", "234.27`"}], ",", "336.`", ",", "6.`", ",", "3.`", ",", "50.8`", ",", "\"\<SF5\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC5081000C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508100C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "44.67`", ",", 
RowBox[{"-", "48.31`"}], ",", "259.4`", ",", "17.`", ",", "2.5`", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508100C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508150C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "39.47899159663866`", ",", 
RowBox[{"-", "49.89`"}], ",", "83.6`", ",", "18.`", ",", "5.`", ",", "50.8`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508150C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508200C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "67.14`", ",", 
RowBox[{"-", "87.57`"}], ",", "234.27`", ",", "12.`", ",", "3.`", ",", "50.8`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508200C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508250C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "78.56`", ",", 
RowBox[{"-", "95.94`"}], ",", "230.7`", ",", "10.`", ",", "3.`", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508250C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508300C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "93.76`", ",", 
RowBox[{"-", "112.21`"}], ",", "280.55`", ",", "8.5`", ",", "3.`", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508300C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508400C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "125.6`", ",", 
RowBox[{"-", "161.5`"}], ",", "376.25`", ",", "6.5`", ",", "3.`", ",", "50.8`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508400C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508500C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "86.1`", ",", 
RowBox[{"-", "103.2`"}], ",", "165.96`", ",", "8.8`", ",", "3.`", ",", "50.8`", ",", "\"\<SF5\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508500C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC508750C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "91.62`", ",", 
RowBox[{"-", "95.94`"}], ",", "130.62`", ",", "8.8`", ",", "3.`", ",", "50.8`", ",", "\"\<SF10\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC508750C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC080030C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.254237288135593`", ",", 
RowBox[{"-", "16.033898305084747`"}], ",", 
RowBox[{"-", "70.41007194244604`"}], ",", "2.250004129282395`", ",", "2.250008926927114`", ",", "8", ",", "\"\<N-PK52A\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC080030C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"AC254125C"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "36.88461538461539`", ",", 
RowBox[{"-", "47.46478873239437`"}], ",", "108.55263157894737`", ",", "5.`", ",", "3.`", ",", "25.4`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6HT\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<AC254125C\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\)|>;


thorlabsCementedDoubletLenses={thorlabsAchromaticCementedDoublets400To700nm,thorlabsAchromaticCementedDoublets400To1100nm,thorlabsAchromaticCementedDoublets650To1050nm,thorlabsAchromaticCementedDoublets650To1700nm};


thorlabsAllSingletLenses={thorlabsDiffractionLimitedPolishedAsphericLenses,thorlabsPolishedAsphericLenses,thorlabsCaF2NegativeMeniscus,thorlabsCaF2PositiveMeniscus,thorlabsCaF2BiConcave,
thorlabsCaF2BiConvex,thorlabsCaF2PlanoConcave,thorlabsCaF2PlanoConvex,thorlabsFusedSilicaNegativeMeniscus,thorlabsFusedSilicaPositiveMeniscus,thorlabsFusedSilicaBiConvex,thorlabsFusedSilicaPlanoConcave,thorlabsFusedSilicaPlanoConvex,thorlabsNSF11BiConcave,thorlabsNSF11PlanoConcave,
thorlabsNBK7BestForm,thorlabsNBK7NegativeMeniscus,thorlabsNBK7PositiveMeniscus,thorlabsNBK7BiConcave,thorlabsNBK7BiConvex,thorlabsNBK7PlanoConcave,thorlabsNBK7PlanoConvex};


thorlabsAllLenses=Join[thorlabsCementedDoubletLenses,thorlabsAllSingletLenses];


(* ::Text:: *)
(*Edmund Optics Aspheric Lenses*)


(*zmx files downloaded on 17.02.2020 from EdmundOptics website, Association[Map[readEdmundOpticsZemaxSingletAsphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\PrecisionAsphericLenses"]]]*)
edmundOpticsPrecisionAspheres=<|"33944"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "29.456953642384107`", ",", 
RowBox[{"-", "0.71`"}], ",", 
RowBox[{"{", 
RowBox[{"5.83981`*^-7", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "5.5`", ",", "25.`", ",", "\n", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<33944\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47725"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "6.627906976744186`", ",", 
RowBox[{"-", "1.076527`"}], ",", 
RowBox[{"{", 
RowBox[{"0.000239604`", ",", "6.414674`*^-7", ",", "7.68584`*^-9", ",", 
RowBox[{"-", "6.476209`*^-11"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "7", ",", "15.`", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47725\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47726"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "8.83695652173913`", ",", 
RowBox[{"-", "1.427973`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0001577441`", ",", 
RowBox[{"-", "6.15204`*^-8"}], ",", "8.528969`*^-10", ",", 
RowBox[{"-", "3.504659`*^-12"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "5.5`", ",", "15.`", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47726\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47727"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "11.045977011494253`", ",", 
RowBox[{"-", "1.832442`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0001166845`", ",", 
RowBox[{"-", "2.15767`*^-7"}], ",", "9.596239`*^-10", ",", 
RowBox[{"-", "2.541401`*^-12"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "4.5`", ",", "15.`", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47727\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47728"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "13.254901960784315`", ",", 
RowBox[{"-", "2.364143`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00009556697`", ",", 
RowBox[{"-", "2.609526`*^-7"}], ",", "1.124621`*^-9", ",", 
RowBox[{"-", "2.998908`*^-12"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "4", ",", "15.`", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47728\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47729"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "11.045977011494253`", ",", 
RowBox[{"-", "0.753209`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00002055097`", ",", "2.629697`*^-8", ",", "1.416305`*^-10", ",", 
RowBox[{"-", "6.623824`*^-13"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "10", ",", "25.`", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47729\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47730"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "14.728070175438596`", ",", 
RowBox[{"-", "1.439137`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00003416666`", ",", 
RowBox[{"-", "6.438044`*^-9"}], ",", "2.323731`*^-11", ",", 
RowBox[{"-", "3.519619`*^-14"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "7.5`", ",", "25.`", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47730\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47731"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "18.41`", ",", 
RowBox[{"-", "1.607913`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00002063455`", ",", 
RowBox[{"-", "7.648977`*^-9"}], ",", "1.117573`*^-11", ",", 
RowBox[{"-", "1.010058`*^-14"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "6.5`", ",", "25.`", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47731\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47732"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "22.091954022988507`", ",", 
RowBox[{"-", "2.271309`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00001954456`", ",", 
RowBox[{"-", "1.756349`*^-8"}], ",", "2.597437`*^-11", ",", 
RowBox[{"-", "2.414068`*^-14"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "6", ",", "25.`", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47732\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"66309"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "9.081632653061224`", ",", 
RowBox[{"-", "0.6249874`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "2.320534`*^-7", ",", 
RowBox[{"-", "2.457574`*^-9"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "200.`"}], ",", "9.6`", ",", "20", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<66309\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"66310"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "12.334677419354838`", ",", 
RowBox[{"-", "0.619614`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "1.292772`*^-8"}], ",", 
RowBox[{"-", "1.932447`*^-10"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "200.`"}], ",", "8", ",", "20", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<66310\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"66311"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "13.621052631578948`", ",", 
RowBox[{"-", "0.6250402`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "3.055889`*^-8", ",", 
RowBox[{"-", "1.438345`*^-10"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "300.`"}], ",", "14.4`", ",", "30", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<66311\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"66312"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "18.51`", ",", 
RowBox[{"-", "0.6220625`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "1.772239`*^-9"}], ",", 
RowBox[{"-", "1.116722`*^-11"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "300.`"}], ",", "11.7`", ",", "30", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<66312\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"66313"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "18.225352112676056`", ",", 
RowBox[{"-", "0.632215`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "4.926605`*^-9", ",", 
RowBox[{"-", "1.73538`*^-11"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "400.`"}], ",", "15.5`", ",", "40", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<66313\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"66314"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "24.680851063829788`", ",", 
RowBox[{"-", "0.622238`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "4.217337`*^-10"}], ",", 
RowBox[{"-", "1.489279`*^-12"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "400.`"}], ",", "15.5`", ",", "40", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<66314\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"66315"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "22.78125`", ",", 
RowBox[{"-", "0.6321058`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "1.614851`*^-9", ",", 
RowBox[{"-", "3.644437`*^-12"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "500.`"}], ",", "19.4`", ",", "50", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<66315\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"66316"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "30.850467289719628`", ",", 
RowBox[{"-", "0.6221342`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "1.379492`*^-10"}], ",", 
RowBox[{"-", "3.12502`*^-13"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "500.`"}], ",", "19.4`", ",", "50", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<66316\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67243"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "6.054054054054054`", ",", 
RowBox[{"-", "1.261245`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0004227332`", ",", "5.228197`*^-7", ",", "1.010405`*^-8", ",", 
RowBox[{"-", "2.409879`*^-10"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "7", ",", "15.`", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67243\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67244"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "8.409090909090908`", ",", 
RowBox[{"-", "1.312935`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0001662932`", ",", "4.824129`*^-8", ",", "8.14988`*^-10", ",", 
RowBox[{"-", "1.105058`*^-11"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "9", ",", "20", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67244\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67245"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "10.090909090909092`", ",", 
RowBox[{"-", "0.7166466`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00002407263`", ",", "2.833095`*^-8", ",", "9.447127`*^-11", ",", 
RowBox[{"-", "2.456638`*^-12"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "11", ",", "25.`", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67245\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67246"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "11.771929824561404`", ",", 
RowBox[{"-", "0.9455748`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0000330058`", ",", "3.973427`*^-8", ",", "5.082571`*^-11", ",", 
RowBox[{"-", "5.317145`*^-13"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "13.5`", ",", "30", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67246\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67247"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "16.817901234567902`", ",", 
RowBox[{"-", "0.9887716`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00001175019`", ",", "5.630664`*^-9", ",", 
RowBox[{"-", "1.060095`*^-12"}], ",", 
RowBox[{"-", "1.392953`*^-14"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "15.5`", ",", "40", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67247\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67248"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "20.18095238095238`", ",", 
RowBox[{"-", "0.9514491`"}], ",", 
RowBox[{"{", 
RowBox[{"6.256875`*^-6", ",", "2.852864`*^-9", ",", 
RowBox[{"-", "9.569919`*^-13"}], ",", 
RowBox[{"-", "2.302468`*^-15"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "20", ",", "50", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67248\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"69852"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "5.044943820224719`", ",", 
RowBox[{"-", "1.055033`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00050966`", ",", "2.254951`*^-6", ",", "8.064402`*^-9", ",", 
RowBox[{"-", "9.079062`*^-10"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "5", ",", "10", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<69852\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"69853"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "5.890909090909091`", ",", 
RowBox[{"-", "1.062472`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0003101282`", ",", "1.265069`*^-6", ",", 
RowBox[{"-", "1.767675`*^-9"}], ",", 
RowBox[{"-", "9.079062`*^-10"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "4", ",", "10", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<69853\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<OHARA\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"69854"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "6.390909090909091`", ",", 
RowBox[{"-", "1.186336`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0003013148`", ",", "3.801752`*^-7", ",", 
RowBox[{"-", "1.46896`*^-9"}], ",", 
RowBox[{"-", "7.154329`*^-11"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "5", ",", "12.5`", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<69854\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"69855"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "7.364077669902913`", ",", 
RowBox[{"-", "1.058074`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0001573925`", ",", "4.159784`*^-7", ",", 
RowBox[{"-", "3.72`*^-10"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "5", ",", "12.5`", ",", "\"\<L-BAL35\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<69855\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\""}], "}"}]}]}], "]"}],
HoldForm]\)|>;


(*zmx files downloaded on 17.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletAsphericalLensFile[#,CharacterEncoding->"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\FusedSilicaAsphericLenses"]]]*)
edmundOpticsFusedSilicaPrecisionAspheres=<|"33947"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "4.9`", ",", 
RowBox[{"-", "4.835`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00227118`", ",", 
RowBox[{"-", "0.0000531763`"}], ",", "1.38332`*^-6", ",", 
RowBox[{"-", "2.1148`*^-8"}], ",", "1.18227`*^-10", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "19.137931034482758`"}], ",", "11.4`", ",", "15.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<33947\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<OHARA\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"33948"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "11.4625`", ",", 
RowBox[{"-", "0.64834`"}], ",", 
RowBox[{"{", 
RowBox[{"6.6501`*^-6", ",", "9.907`*^-9", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "4.36`", ",", "15.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<33948\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<OHARA\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"33949"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "8.92`", ",", 
RowBox[{"-", "1.53`"}], ",", 
RowBox[{"{", 
RowBox[{"0.000135`", ",", 
RowBox[{"-", "9.631`*^-8"}], ",", "5.7`*^-9", ",", 
RowBox[{"-", "6.7129`*^-11"}], ",", "1.76`*^-13", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "38.177083333333336`"}], ",", "14.8`", ",", "25.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<33949\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<OHARA\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"33950"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "22.93`", ",", 
RowBox[{"-", "0.62`"}], ",", 
RowBox[{"{", 
RowBox[{"4.8277`*^-7", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "5.13`", ",", "25.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<33950\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<OHARA\>\"", ",", "\"\<MISC\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"48534"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "6.876923076923077`", ",", 
RowBox[{"-", "2.076597589618`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0005787995098921`", ",", 
RowBox[{"-", "3.16260950124`*^-6"}], ",", "3.471802858882`*^-8", ",", 
RowBox[{"-", "1.019232799956`*^-10"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "7.25`", ",", "15.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<48534\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"48535"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "9.169014084507042`", ",", 
RowBox[{"-", "2.659391201185`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0003363514904923`", ",", 
RowBox[{"-", "2.146864041489`*^-6"}], ",", "1.809962924478`*^-8", ",", 
RowBox[{"-", "7.025981245851`*^-11"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "6", ",", "15.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<48535\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"48536"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "11.462068965517242`", ",", 
RowBox[{"-", "1.661221569159`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00009167421464497`", ",", 
RowBox[{"-", "7.166361952686`*^-8"}], ",", "3.556473766174`*^-10", ",", 
RowBox[{"-", "1.041048498674`*^-13"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "9.75`", ",", "25.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<48536\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"48537"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "13.754098360655737`", ",", 
RowBox[{"-", "2.050190672931`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00007122874791803`", ",", 
RowBox[{"-", "1.068822220343`*^-7"}], ",", "3.288486540153`*^-10", ",", 
RowBox[{"-", "3.774341961392`*^-13"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "8.5`", ",", "25.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<48537\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67264"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "5.73109243697479`", ",", 
RowBox[{"-", "1.123451578476`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0003801254420993`", ",", "2.946222573223`*^-6", ",", 
RowBox[{"-", "1.655839259764`*^-8"}], ",", "5.34969121154`*^-10", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "9", ",", "15.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67264\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67265"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "9.17`", ",", 
RowBox[{"-", "1.102626092761`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00008791869084293`", ",", "3.051652280989`*^-7", ",", 
RowBox[{"-", "7.950596944423`*^-10"}], ",", "8.04204278762`*^-12", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "14", ",", "25.`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67265\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67266"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "18.338983050847457`", ",", 
RowBox[{"-", "1.099272370277`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00001094492238508`", ",", "9.288685889257`*^-9", ",", 
RowBox[{"-", "5.645807060379`*^-12"}], ",", "1.501009854933`*^-14", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "27.5`", ",", "50", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67266\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67267"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "22.923076923076923`", ",", 
RowBox[{"-", "1.215775829594`"}], ",", 
RowBox[{"{", 
RowBox[{"6.880040925902`*^-6", ",", "1.315975173107`*^-9", ",", "6.062406507035`*^-13", ",", "5.160185004234`*^-16", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "19.5`", ",", "50", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67267\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"67268"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "27.50793650793651`", ",", 
RowBox[{"-", "1.471923385095`"}], ",", 
RowBox[{"{", 
RowBox[{"5.474309441802`*^-6", ",", 
RowBox[{"-", "2.150776253741`*^-10"}], ",", "4.540082400701`*^-13", ",", 
RowBox[{"-", "3.525982474262`*^-18"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "17", ",", "50", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67268\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"87973"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "3.668085106382979`", ",", 
RowBox[{"-", "0.6304670790461`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0003025727227586`", ",", "0.00001331749466913`", ",", 
RowBox[{"-", "4.509731393245`*^-7"}], ",", "6.244402863406`*^-8", ",", 
RowBox[{"-", "2.297646765956`*^-9"}], ",", "5.046328544243`*^-11", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "8", ",", "10", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<87973\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"87974"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "4.584905660377358`", ",", 
RowBox[{"-", "0.6329059825138`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0001282321528676`", ",", "1.521181585038`*^-6", ",", "3.394006130875`*^-8", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "6", ",", "10", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<87974\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"87975"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "4.584905660377358`", ",", 
RowBox[{"-", "0.6771756320983`"}], ",", 
RowBox[{"{", 
RowBox[{"0.0002100357487315`", ",", "2.316792329526`*^-6", ",", "1.222944991019`*^-7", ",", 
RowBox[{"-", "2.000160028566`*^-9"}], ",", "5.453531073969`*^-11", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "8", ",", "12.5`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<87975\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"87976"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "5.73109243697479`", ",", 
RowBox[{"-", "0.65491254822`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00007401037196855`", ",", "5.564214987626`*^-7", ",", "6.86488734643`*^-9", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "6", ",", "12.5`", ",", "\"\<F_SILICA\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<87976\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\)|>;


(*zmx files downloaded on 17.02.2020 from EdmundOptics website, Association[Map[readEdmundOpticsZemaxSingletAsphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\Lambda40Aspheres"]]]*)
edmundOpticsLambda40Aspheres=<|"12428"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "12.077922077922079`", ",", 
RowBox[{"-", "0.9820666`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00002608586`", ",", 
RowBox[{"-", "5.633708`*^-9"}], ",", 
RowBox[{"-", "1.621792`*^-10"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "8.623`", ",", "15.`", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<12428\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"12429"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "15.097087378640778`", ",", "0.0204531`", ",", 
RowBox[{"{", 
RowBox[{
RowBox[{"-", "0.00002464272`"}], ",", 
RowBox[{"-", "7.747754`*^-8"}], ",", 
RowBox[{"-", "4.303751`*^-10"}], ",", 
RowBox[{"-", "8.575912`*^-16"}], ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "8", ",", "15.`", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<12429\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"12430"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "11.63`", ",", 
RowBox[{"-", "0.901164`"}], ",", 
RowBox[{"{", 
RowBox[{"0.00002773168`", ",", "5.678751`*^-8", ",", 
RowBox[{"-", "3.671194`*^-11"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "8.79`", ",", "15.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<12430\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"12438"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "19.38`", ",", 
RowBox[{"-", "0.6698934`"}], ",", 
RowBox[{"{", 
RowBox[{"1.876737`*^-6", ",", "8.930244`*^-10", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "9.721`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<12438\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"12439"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "25.84`", ",", 
RowBox[{"-", "0.800424`"}], ",", 
RowBox[{"{", 
RowBox[{"1.643994`*^-6", ",", "5.887865`*^-10", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "7.24`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<12439\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"12445"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "26.91`", ",", 
RowBox[{"-", "0.763317`"}], ",", 
RowBox[{"{", 
RowBox[{"1.1304`*^-6", ",", "2.028051`*^-10", ",", 
RowBox[{"-", "9.363066`*^-13"}], ",", "1.944793`*^-15", ",", 
RowBox[{"-", "3.345788`*^-18"}], ",", "2.852466`*^-21", ",", 
RowBox[{"-", "9.954218`*^-25"}]}], "}"}], ",", "\[Infinity]", ",", "14.7`", ",", "40", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<12445\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"12448"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "40.25892857142857`", ",", 
RowBox[{"-", "0.770465`"}], ",", 
RowBox[{"{", 
RowBox[{"1.478988`*^-7", ",", 
RowBox[{"-", "7.467807`*^-11"}], ",", 
RowBox[{"-", "5.681265`*^-14"}], ",", "1.782653`*^-17", ",", "0", ",", "0", ",", "0"}], "}"}], ",", "\[Infinity]", ",", "13.058`", ",", "50", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<12448\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\)|>;


(*zmx files downloaded on 17.02.2020 from EdmundOptics website, Association[Map[readEdmundOpticsZemaxSingletAsphericalLensFile[#,CharacterEncoding->"Unicode",TreatSurface1lAwaysAsFirstFaceOfLens->True]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\BestFormAsphericLenses"]]]*)
edmundOpticsBestFormAspheres=<|"89431"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "12.987012987012987`", ",", 
RowBox[{"-", "2.3087969`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "6.61`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89431\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89432"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "25.97`", ",", 
RowBox[{"-", "2.308796930418`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "4.3`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89432\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89433"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "38.96`", ",", 
RowBox[{"-", "2.308796930477`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "4.25`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89433\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89434"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "51.95`", ",", 
RowBox[{"-", "2.308796929145`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "4.05`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89434\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89435"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "12.876923076923077`", ",", 
RowBox[{"-", "2.295474533381`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "6.61`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89435\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89437"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "38.63`", ",", 
RowBox[{"-", "2.29547453341`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "4.25`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89437\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89438"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "51.51`", ",", 
RowBox[{"-", "2.295474533072`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "4.05`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89438\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89439"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "12.665903890160184`", ",", 
RowBox[{"-", "2.269948425669`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "6.61`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89439\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89440"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "25.33`", ",", 
RowBox[{"-", "2.269948425418`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "4.3`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89440\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89441"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "38.`", ",", 
RowBox[{"-", "2.269948425676`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "4.25`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89441\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"89442"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createAsphericLens", "[", 
RowBox[{"0", ",", "50.66`", ",", 
RowBox[{"-", "2.269948425074`"}], ",", 
RowBox[{"{", 
RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
RowBox[{"-", "\[Infinity]"}], ",", "4.05`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", 
RowBox[{"Reverse", "->", "True"}], ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89442\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\""}], "}"}]}]}], "]"}],
HoldForm]\)|>;


(* ::Text:: *)
(*Edmund Optics Spherical Lenses*)


(* ::Input::Initialization:: *)
(*zemax files downloaded on 16.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\FusedSIlicaBiConcave"]]]*)edmundOpticsFusedOpticsBiConcave=<|"48054"->OpenRayTracer`createSphericalLens[0,-18.648`,18.648`,2,12,"C79-80",OpenRayTracer`PartId->"48054",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48055"->OpenRayTracer`createSphericalLens[0,-27.817941952506597`,27.817901234567902`,2,12,"C79-80",OpenRayTracer`PartId->"48055",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48056"->OpenRayTracer`createSphericalLens[0,-32.4040404040404`,32.4040404040404`,2,12,"C79-80",OpenRayTracer`PartId->"48056",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48057"->OpenRayTracer`createSphericalLens[0,-41.57303370786517`,41.57303370786517`,2,12,"C79-80",OpenRayTracer`PartId->"48057",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48327"->OpenRayTracer`createSphericalLens[0,-46.235955056179776`,46.235955056179776`,2.5`,25.`,"C79-80",OpenRayTracer`PartId->"48327",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48328"->OpenRayTracer`createSphericalLens[0,-92.08294930875576`,92.08290155440415`,2.5`,25.`,"C79-80",OpenRayTracer`PartId->"48328",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48329"->OpenRayTracer`createSphericalLens[0,-115.00704225352112`,115.00704225352112`,2.5`,25.`,"C79-80",OpenRayTracer`PartId->"48329",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 16.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\BiConcaveLenses"]]]*)edmundOpticsBiConcave=<|"32986"->OpenRayTracer`createSphericalLens[0,-9.74`,9.74`,1.5`,6,"N-SF11",OpenRayTracer`PartId->"32986",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32988"->OpenRayTracer`createSphericalLens[0,-19.47`,19.47`,3,12,"N-SF11",OpenRayTracer`PartId->"32988",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32990"->OpenRayTracer`createSphericalLens[0,-31.82`,31.82`,2,20,"N-SF11",OpenRayTracer`PartId->"32990",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32992"->OpenRayTracer`createSphericalLens[0,-39.78`,39.78`,2.5`,25.`,"N-SF11",OpenRayTracer`PartId->"32992",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32994"->OpenRayTracer`createSphericalLens[0,-104.2`,104.2`,5,25.`,"N-BK7",OpenRayTracer`PartId->"32994",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32996"->OpenRayTracer`createSphericalLens[0,-52.1`,52.1`,2.5`,25.`,"N-BK7",OpenRayTracer`PartId->"32996",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48336"->OpenRayTracer`createSphericalLens[0,-14.6`,14.6`,2.25`,9.`,"N-SF11",OpenRayTracer`PartId->"48336",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48681"->OpenRayTracer`createSphericalLens[0,-12.570945945945946`,12.57090909090909`,1,6,"N-BK7",OpenRayTracer`PartId->"48681",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48682"->OpenRayTracer`createSphericalLens[0,-18.856953642384106`,18.856902356902356`,1.5`,9.`,"N-BK7",OpenRayTracer`PartId->"48682",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48683"->OpenRayTracer`createSphericalLens[0,-25.141935483870967`,25.141935483870967`,2,12,"N-BK7",OpenRayTracer`PartId->"48683",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48932"->OpenRayTracer`createSphericalLens[0,-9.47`,9.47`,1,6,"N-BK7",OpenRayTracer`PartId->"48932",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48933"->OpenRayTracer`createSphericalLens[0,-19.15702479338843`,19.15702479338843`,1.5`,9.`,"N-SF11",OpenRayTracer`PartId->"48933",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48934"->OpenRayTracer`createSphericalLens[0,-18.9390243902439`,18.9390243902439`,2,12,"N-BK7",OpenRayTracer`PartId->"48934",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 16.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\CaF2BiConvexLenses"]]]*)edmundOpticsCaF2BiConvex=<|"47056"->OpenRayTracer`createSphericalLens[0,18.37`,-18.37`,12.2`,25.4`,"CAF2",OpenRayTracer`PartId->"47056",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"47057"->OpenRayTracer`createSphericalLens[0,39.61`,-39.61`,6.2`,25.4`,"CAF2",OpenRayTracer`PartId->"47057",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"47058"->OpenRayTracer`createSphericalLens[0,60.12`,-60.12`,4.7`,25.4`,"CAF2",OpenRayTracer`PartId->"47058",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"47059"->OpenRayTracer`createSphericalLens[0,79.07`,-79.07`,4.1`,25.4`,"CAF2",OpenRayTracer`PartId->"47059",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}],"48018"->OpenRayTracer`createSphericalLens[0,49.89`,-49.89`,5.3`,25.4`,"CAF2",OpenRayTracer`PartId->"48018",OpenRayTracer`GlassCatalogueData->{"SCHOTT","INFRARED","MISC"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 16.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\FusedSilicaBiConvex"]]]*)edmundOpticsFusedSilicaBiConvex=<|"8083"->OpenRayTracer`createSphericalLens[0,17.05`,-17.05`,7.64`,18,"C79-80",OpenRayTracer`PartId->"8083",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"8084"->OpenRayTracer`createSphericalLens[0,35.99`,-35.99`,4.29`,18,"C79-80",OpenRayTracer`PartId->"8084",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48034"->OpenRayTracer`createSphericalLens[0,9.74`,-9.74`,7.09`,12,"C79-80",OpenRayTracer`PartId->"48034",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48035"->OpenRayTracer`createSphericalLens[0,17.63`,-17.63`,4.37`,12,"C79-80",OpenRayTracer`PartId->"48035",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48036"->OpenRayTracer`createSphericalLens[0,26.943089430894307`,-26.943089430894307`,3.52`,12,"C79-80",OpenRayTracer`PartId->"48036",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48037"->OpenRayTracer`createSphericalLens[0,31.566037735849058`,-31.566037735849058`,3.29`,12,"C79-80",OpenRayTracer`PartId->"48037",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48038"->OpenRayTracer`createSphericalLens[0,40.784946236559136`,-40.784946236559136`,2.99`,12,"C79-80",OpenRayTracer`PartId->"48038",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48039"->OpenRayTracer`createSphericalLens[0,68.36206896551724`,-68.36206896551724`,2.59`,12,"C79-80",OpenRayTracer`PartId->"48039",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48293"->OpenRayTracer`createSphericalLens[0,21.06`,-21.06`,10.9`,25.`,"C79-80",OpenRayTracer`PartId->"48293",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48294"->OpenRayTracer`createSphericalLens[0,44.94`,-44.94`,5.66`,25.`,"C79-80",OpenRayTracer`PartId->"48294",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48295"->OpenRayTracer`createSphericalLens[0,54.13592233009709`,-54.13592233009709`,5.52`,25.`,"C79-80",OpenRayTracer`PartId->"48295",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48296"->OpenRayTracer`createSphericalLens[0,68.54491017964072`,-68.54494382022472`,4.87`,25.`,"C79-80",OpenRayTracer`PartId->"48296",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48297"->OpenRayTracer`createSphericalLens[0,91.09401709401709`,-91.09401709401709`,3.78`,25.`,"C79-80",OpenRayTracer`PartId->"48297",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48298"->OpenRayTracer`createSphericalLens[0,114.075`,-114.075`,3.42`,25.`,"C79-80",OpenRayTracer`PartId->"48298",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48299"->OpenRayTracer`createSphericalLens[0,136.95698924731184`,-136.95698924731184`,3.68`,25.`,"C79-80",OpenRayTracer`PartId->"48299",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48300"->OpenRayTracer`createSphericalLens[0,182.85106382978722`,-182.85106382978722`,3.38`,25.`,"C79-80",OpenRayTracer`PartId->"48300",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48301"->OpenRayTracer`createSphericalLens[0,228.725`,-228.725`,3.21`,25.`,"C79-80",OpenRayTracer`PartId->"48301",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48302"->OpenRayTracer`createSphericalLens[0,274.60109289617486`,-274.6010638297872`,3.09`,25.`,"C79-80",OpenRayTracer`PartId->"48302",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48303"->OpenRayTracer`createSphericalLens[0,457.9329268292683`,-457.9329608938547`,3.35`,25.`,"C79-80",OpenRayTracer`PartId->"48303",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49238"->OpenRayTracer`createSphericalLens[0,4.76`,-4.76`,4.1`,6,"C79-80",OpenRayTracer`PartId->"49238",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49239"->OpenRayTracer`createSphericalLens[0,7.8`,-7.8`,2.7`,6,"C79-80",OpenRayTracer`PartId->"49239",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49240"->OpenRayTracer`createSphericalLens[0,10.65`,-10.65`,2.2`,6,"C79-80",OpenRayTracer`PartId->"49240",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49241"->OpenRayTracer`createSphericalLens[0,7.39`,-7.39`,4.9`,9.`,"C79-80",OpenRayTracer`PartId->"49241",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49242"->OpenRayTracer`createSphericalLens[0,11.84`,-11.84`,3.3`,9.`,"C79-80",OpenRayTracer`PartId->"49242",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49243"->OpenRayTracer`createSphericalLens[0,16.09`,-16.09`,2.6`,9.`,"C79-80",OpenRayTracer`PartId->"49243",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49244"->OpenRayTracer`createSphericalLens[0,13.01`,-13.01`,4.5`,12,"C79-80",OpenRayTracer`PartId->"49244",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49245"->OpenRayTracer`createSphericalLens[0,21.51`,-21.51`,3.1`,12,"C79-80",OpenRayTracer`PartId->"49245",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49983"->OpenRayTracer`createSphericalLens[0,16.18`,-16.18`,2,6,"C79-80",OpenRayTracer`PartId->"49983",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49984"->OpenRayTracer`createSphericalLens[0,21.69`,-21.69`,2,6,"C79-80",OpenRayTracer`PartId->"49984",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49985"->OpenRayTracer`createSphericalLens[0,24.36`,-24.36`,2.5`,9.`,"C79-80",OpenRayTracer`PartId->"49985",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49986"->OpenRayTracer`createSphericalLens[0,32.69`,-32.69`,2,9.`,"C79-80",OpenRayTracer`PartId->"49986",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 16.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\BiConvexLenses"]]]*)edmundOpticsBiConvex=<|"32012"->OpenRayTracer`createSphericalLens[0,18.01`,-18.01`,3.48`,12,"N-BK7",OpenRayTracer`PartId->"32012",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32014"->OpenRayTracer`createSphericalLens[0,15.34`,-15.34`,3.85`,12,"N-SF5",OpenRayTracer`PartId->"32014",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32016"->OpenRayTracer`createSphericalLens[0,13.46`,-13.46`,2.75`,9.`,"N-BK7",OpenRayTracer`PartId->"32016",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32018"->OpenRayTracer`createSphericalLens[0,11.38`,-11.38`,3.45`,9.`,"N-SF5",OpenRayTracer`PartId->"32018",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32020"->OpenRayTracer`createSphericalLens[0,8.91`,-8.91`,2.25`,6,"N-BK7",OpenRayTracer`PartId->"32020",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32021"->OpenRayTracer`createSphericalLens[0,7.59`,-7.59`,2.54`,6,"N-SF5",OpenRayTracer`PartId->"32021",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32022"->OpenRayTracer`createSphericalLens[0,4.29`,-4.29`,2,3.`,"N-BK7",OpenRayTracer`PartId->"32022",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32023"->OpenRayTracer`createSphericalLens[0,3.5`,-3.5`,2.3`,3.`,"N-SF5",OpenRayTracer`PartId->"32023",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32489"->OpenRayTracer`createSphericalLens[0,31.94`,-31.94`,8,25.`,"N-SF5",OpenRayTracer`PartId->"32489",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32624"->OpenRayTracer`createSphericalLens[0,50.8`,-50.8`,5,25.`,"N-BK7",OpenRayTracer`PartId->"32624",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32626"->OpenRayTracer`createSphericalLens[0,76.66`,-76.66`,3.5`,25.`,"N-BK7",OpenRayTracer`PartId->"32626",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32705"->OpenRayTracer`createSphericalLens[0,30.5`,-30.5`,4,15.`,"N-BK7",OpenRayTracer`PartId->"32705",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32707"->OpenRayTracer`createSphericalLens[0,27.08`,-27.08`,5.5`,18,"N-BK7",OpenRayTracer`PartId->"32707",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32709"->OpenRayTracer`createSphericalLens[0,36.6`,-36.6`,4.8`,18,"N-BK7",OpenRayTracer`PartId->"32709",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32711"->OpenRayTracer`createSphericalLens[0,40.67`,-40.67`,5.3`,20,"N-BK7",OpenRayTracer`PartId->"32711",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32717"->OpenRayTracer`createSphericalLens[0,103.`,-103.`,4,25.`,"N-BK7",OpenRayTracer`PartId->"32717",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32867"->OpenRayTracer`createSphericalLens[0,24.36`,-24.36`,2.6`,6,"N-BK7",OpenRayTracer`PartId->"32867",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32869"->OpenRayTracer`createSphericalLens[0,36.65`,-36.65`,3.3`,12,"N-BK7",OpenRayTracer`PartId->"32869",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32871"->OpenRayTracer`createSphericalLens[0,73.97`,-73.97`,2.8`,12,"N-BK7",OpenRayTracer`PartId->"32871",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32873"->OpenRayTracer`createSphericalLens[0,128.65`,-128.65`,3.5`,25.`,"N-BK7",OpenRayTracer`PartId->"32873",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32875"->OpenRayTracer`createSphericalLens[0,154.52`,-154.52`,3.4`,25.`,"N-BK7",OpenRayTracer`PartId->"32875",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32877"->OpenRayTracer`createSphericalLens[0,180.4`,-180.4`,3.2`,25.`,"N-BK7",OpenRayTracer`PartId->"32877",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32879"->OpenRayTracer`createSphericalLens[0,71.08`,-71.08`,7.5`,35.`,"N-BK7",OpenRayTracer`PartId->"32879",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32938"->OpenRayTracer`createSphericalLens[0,51.16`,-51.16`,3,15.`,"N-BK7",OpenRayTracer`PartId->"32938",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32964"->OpenRayTracer`createSphericalLens[0,11.98`,-11.98`,2.4`,6,"N-BK7",OpenRayTracer`PartId->"32964",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32966"->OpenRayTracer`createSphericalLens[0,18.22`,-18.22`,2.2`,6,"N-BK7",OpenRayTracer`PartId->"32966",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32968"->OpenRayTracer`createSphericalLens[0,18.15`,-18.15`,2.6`,9.`,"N-BK7",OpenRayTracer`PartId->"32968",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32978"->OpenRayTracer`createSphericalLens[0,74.78`,-74.78`,16,50,"N-SF11",OpenRayTracer`PartId->"32978",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32980"->OpenRayTracer`createSphericalLens[0,101.63`,-101.63`,10,50,"N-BK7",OpenRayTracer`PartId->"32980",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32982"->OpenRayTracer`createSphericalLens[0,153.49`,-153.49`,9,50,"N-BK7",OpenRayTracer`PartId->"32982",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33388"->OpenRayTracer`createSphericalLens[0,24.107913669064747`,-24.107913669064747`,4,12,"N-BK7",OpenRayTracer`PartId->"33388",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33394"->OpenRayTracer`createSphericalLens[0,205.86507936507937`,-205.86507936507937`,5,25.`,"N-BK7",OpenRayTracer`PartId->"33394",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33406"->OpenRayTracer`createSphericalLens[0,101.98`,-101.98`,8,40,"N-BK7",OpenRayTracer`PartId->"33406",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33412"->OpenRayTracer`createSphericalLens[0,205.35`,-205.35`,8,40,"N-BK7",OpenRayTracer`PartId->"33412",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33419"->OpenRayTracer`createSphericalLens[0,81.3`,-81.3`,8,40,"N-BK7",OpenRayTracer`PartId->"33419",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45087"->OpenRayTracer`createSphericalLens[0,30.36`,-30.36`,3.7`,12,"N-BK7",OpenRayTracer`PartId->"45087",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45099"->OpenRayTracer`createSphericalLens[0,22.51`,-22.51`,4.5`,15.`,"N-SF11",OpenRayTracer`PartId->"45099",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45101"->OpenRayTracer`createSphericalLens[0,34.36`,-34.36`,4.2`,15.`,"N-SF11",OpenRayTracer`PartId->"45101",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45129"->OpenRayTracer`createSphericalLens[0,5.88`,-5.88`,1.8`,3.`,"N-BK7",OpenRayTracer`PartId->"45129",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45131"->OpenRayTracer`createSphericalLens[0,9.02`,-9.02`,1.6`,3.`,"N-BK7",OpenRayTracer`PartId->"45131",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45133"->OpenRayTracer`createSphericalLens[0,30.61`,-30.61`,2.3`,6,"N-BK7",OpenRayTracer`PartId->"45133",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45155"->OpenRayTracer`createSphericalLens[0,27.43`,-27.43`,2.74`,9.`,"N-BK7",OpenRayTracer`PartId->"45155",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45157"->OpenRayTracer`createSphericalLens[0,36.77`,-36.77`,2.55`,9.`,"N-BK7",OpenRayTracer`PartId->"45157",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45159"->OpenRayTracer`createSphericalLens[0,49.14`,-49.14`,2.74`,12,"N-BK7",OpenRayTracer`PartId->"45159",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45161"->OpenRayTracer`createSphericalLens[0,35.09`,-35.09`,6.6`,25.`,"N-BK7",OpenRayTracer`PartId->"45161",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45163"->OpenRayTracer`createSphericalLens[0,50.56`,-50.56`,6.55`,30,"N-BK7",OpenRayTracer`PartId->"45163",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45165"->OpenRayTracer`createSphericalLens[0,76.67`,-76.67`,4.96`,30,"N-BK7",OpenRayTracer`PartId->"45165",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45167"->OpenRayTracer`createSphericalLens[0,60.56`,-60.56`,8.8`,40,"N-BK7",OpenRayTracer`PartId->"45167",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45169"->OpenRayTracer`createSphericalLens[0,205.86`,-205.86`,5.05`,50,"N-BK7",OpenRayTracer`PartId->"45169",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45171"->OpenRayTracer`createSphericalLens[0,257.57`,-257.57`,4.93`,50,"N-BK7",OpenRayTracer`PartId->"45171",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45176"->OpenRayTracer`createSphericalLens[0,6.58`,-6.58`,2.6`,4.5`,"N-LASF44",OpenRayTracer`PartId->"45176",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45185"->OpenRayTracer`createSphericalLens[0,8.88`,-8.88`,2.38`,4.5`,"N-BK7",OpenRayTracer`PartId->"45185",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45290"->OpenRayTracer`createSphericalLens[0,36.87`,-36.87`,2,6,"N-BK7",OpenRayTracer`PartId->"45290",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45292"->OpenRayTracer`createSphericalLens[0,46.08`,-46.08`,2.5`,9.`,"N-BK7",OpenRayTracer`PartId->"45292",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45294"->OpenRayTracer`createSphericalLens[0,30.08`,-30.08`,5.3`,20,"N-BK7",OpenRayTracer`PartId->"45294",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45296"->OpenRayTracer`createSphericalLens[0,40.42`,-40.42`,5.3`,25.`,"N-BK7",OpenRayTracer`PartId->"45296",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63530"->OpenRayTracer`createSphericalLens[0,6.18`,-6.18`,2.5`,5.`,"N-SF5",OpenRayTracer`PartId->"63530",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63531"->OpenRayTracer`createSphericalLens[0,9.98`,-9.98`,2,5.`,"N-BK7",OpenRayTracer`PartId->"63531",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63532"->OpenRayTracer`createSphericalLens[0,15.16`,-15.16`,2,5.`,"N-BK7",OpenRayTracer`PartId->"63532",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63533"->OpenRayTracer`createSphericalLens[0,20.41`,-20.41`,1.5`,5.`,"N-BK7",OpenRayTracer`PartId->"63533",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63534"->OpenRayTracer`createSphericalLens[0,25.58`,-25.58`,1.5`,5.`,"N-BK7",OpenRayTracer`PartId->"63534",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63535"->OpenRayTracer`createSphericalLens[0,12.71`,-12.71`,3.5`,10,"N-SF5",OpenRayTracer`PartId->"63535",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63536"->OpenRayTracer`createSphericalLens[0,14.88`,-14.88`,3.5`,10,"N-BK7",OpenRayTracer`PartId->"63536",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63537"->OpenRayTracer`createSphericalLens[0,20.24`,-20.24`,2.5`,10,"N-BK7",OpenRayTracer`PartId->"63537",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63538"->OpenRayTracer`createSphericalLens[0,25.41`,-25.41`,2.5`,10,"N-BK7",OpenRayTracer`PartId->"63538",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63539"->OpenRayTracer`createSphericalLens[0,30.58`,-30.58`,2.5`,10,"N-BK7",OpenRayTracer`PartId->"63539",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63540"->OpenRayTracer`createSphericalLens[0,40.91`,-40.91`,2.5`,10,"N-BK7",OpenRayTracer`PartId->"63540",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63541"->OpenRayTracer`createSphericalLens[0,51.25`,-51.25`,2.5`,10,"N-BK7",OpenRayTracer`PartId->"63541",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63542"->OpenRayTracer`createSphericalLens[0,102.93`,-102.93`,2.5`,10,"N-BK7",OpenRayTracer`PartId->"63542",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63543"->OpenRayTracer`createSphericalLens[0,30.6`,-30.6`,3.5`,15.`,"N-SF11",OpenRayTracer`PartId->"63543",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63544"->OpenRayTracer`createSphericalLens[0,25.23`,-25.23`,3.5`,15.`,"N-BK7",OpenRayTracer`PartId->"63544",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63545"->OpenRayTracer`createSphericalLens[0,45.99`,-45.99`,3,15.`,"N-BK7",OpenRayTracer`PartId->"63545",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63546"->OpenRayTracer`createSphericalLens[0,61.5`,-61.5`,3,15.`,"N-BK7",OpenRayTracer`PartId->"63546",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63547"->OpenRayTracer`createSphericalLens[0,27.22`,-27.22`,4.5`,18,"N-SF11",OpenRayTracer`PartId->"63547",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63550"->OpenRayTracer`createSphericalLens[0,30.25`,-30.25`,5,20,"N-SF11",OpenRayTracer`PartId->"63550",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63551"->OpenRayTracer`createSphericalLens[0,38.34`,-38.34`,4,20,"N-SF11",OpenRayTracer`PartId->"63551",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63552"->OpenRayTracer`createSphericalLens[0,51.08`,-51.08`,3.5`,20,"N-BK7",OpenRayTracer`PartId->"63552",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63553"->OpenRayTracer`createSphericalLens[0,61.33`,-61.33`,4,20,"N-BK7",OpenRayTracer`PartId->"63553",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63554"->OpenRayTracer`createSphericalLens[0,82.`,-82.`,4,20,"N-BK7",OpenRayTracer`PartId->"63554",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63555"->OpenRayTracer`createSphericalLens[0,39.22`,-39.22`,5.5`,25.`,"N-SF5",OpenRayTracer`PartId->"63555",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63556"->OpenRayTracer`createSphericalLens[0,45.61`,-45.61`,6.5`,30,"N-SF11",OpenRayTracer`PartId->"63556",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63557"->OpenRayTracer`createSphericalLens[0,61.15`,-61.15`,5,30,"N-BK7",OpenRayTracer`PartId->"63557",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63558"->OpenRayTracer`createSphericalLens[0,102.5`,-102.5`,5,30,"N-BK7",OpenRayTracer`PartId->"63558",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63559"->OpenRayTracer`createSphericalLens[0,123.17`,-123.17`,5,30,"N-BK7",OpenRayTracer`PartId->"63559",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63560"->OpenRayTracer`createSphericalLens[0,60.85`,-60.85`,8.5`,40,"N-SF11",OpenRayTracer`PartId->"63560",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63561"->OpenRayTracer`createSphericalLens[0,122.83`,-122.83`,7,40,"N-BK7",OpenRayTracer`PartId->"63561",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 12.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\CaF2PlanoConvex"]]]*)edmundOpticsCaF2PlanoConvex=<|"88160"->OpenRayTracer`createSphericalLens[0,8.31791907514451`,\[Infinity],4,12.5`,"CAF2",OpenRayTracer`PartId->"88160",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}],"88161"->OpenRayTracer`createSphericalLens[0,11.552083333333334`,\[Infinity],3.5`,12.5`,"CAF2",OpenRayTracer`PartId->"88161",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}],"88162"->OpenRayTracer`createSphericalLens[0,23.104938271604937`,\[Infinity],2,12.5`,"CAF2",OpenRayTracer`PartId->"88162",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}],"88163"->OpenRayTracer`createSphericalLens[0,16.17391304347826`,\[Infinity],7.5`,25.`,"CAF2",OpenRayTracer`PartId->"88163",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}],"88164"->OpenRayTracer`createSphericalLens[0,23.104938271604937`,\[Infinity],5,25.`,"CAF2",OpenRayTracer`PartId->"88164",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}],"88165"->OpenRayTracer`createSphericalLens[0,34.65693430656934`,\[Infinity],4,25.`,"CAF2",OpenRayTracer`PartId->"88165",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}],"88166"->OpenRayTracer`createSphericalLens[0,46.21`,\[Infinity],4,25.`,"CAF2",OpenRayTracer`PartId->"88166",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}],"88167"->OpenRayTracer`createSphericalLens[0,34.65693430656934`,\[Infinity],12.5`,50,"CAF2",OpenRayTracer`PartId->"88167",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}],"88168"->OpenRayTracer`createSphericalLens[0,46.21`,\[Infinity],9,50,"CAF2",OpenRayTracer`PartId->"88168",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}],"88169"->OpenRayTracer`createSphericalLens[0,69.31395348837209`,\[Infinity],7,50,"CAF2",OpenRayTracer`PartId->"88169",OpenRayTracer`GlassCatalogueData->{"SCHOTT","CDGM","MISC"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 12.02.2020 from EdmundOptics website,Association[Join[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\LargePlanoConvexLenses"]],Map[readEdmundOpticsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\LargePlanoConvexLenses\\ASCII"]]]]*)edmundOpticsFusedSilicaLargePlanoConvex=<|"27510"->OpenRayTracer`createSphericalLens[0,232.6`,\[Infinity],17.5`,150,"N-BK7",OpenRayTracer`PartId->"27510",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27511"->OpenRayTracer`createSphericalLens[0,310.2`,\[Infinity],14.2`,150,"N-BK7",OpenRayTracer`PartId->"27511",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27513"->OpenRayTracer`createSphericalLens[0,207.`,\[Infinity],31,200,"N-BK7",OpenRayTracer`PartId->"27513",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27514"->OpenRayTracer`createSphericalLens[0,310.`,\[Infinity],21.6`,200,"N-BK7",OpenRayTracer`PartId->"27514",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27515"->OpenRayTracer`createSphericalLens[0,413.5`,\[Infinity],17.3`,200,"N-BK7",OpenRayTracer`PartId->"27515",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67187"->OpenRayTracer`createSphericalLens[0,77.57065217391305`,\[Infinity],21,100,"N-BK7",OpenRayTracer`PartId->"67187",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"67191"->OpenRayTracer`createSphericalLens[0,103.36`,\[Infinity],34.75`,150,"N-BK7",OpenRayTracer`PartId->"67191",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"67199"->OpenRayTracer`createSphericalLens[0,258.4`,\[Infinity],35.5`,250,"N-BK7",OpenRayTracer`PartId->"67199",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"27501"->OpenRayTracer`createSphericalLens[0,103.5`,\[Infinity],17,100,"N-BK7",OpenRayTracer`PartId->"27501",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27502"->OpenRayTracer`createSphericalLens[0,155.`,\[Infinity],12.5`,100,"N-BK7",OpenRayTracer`PartId->"27502",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27503"->OpenRayTracer`createSphericalLens[0,206.7`,\[Infinity],10,100,"N-BK7",OpenRayTracer`PartId->"27503",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27505"->OpenRayTracer`createSphericalLens[0,129.2`,\[Infinity],20,125.`,"N-BK7",OpenRayTracer`PartId->"27505",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27506"->OpenRayTracer`createSphericalLens[0,193.8`,\[Infinity],14.4`,125.`,"N-BK7",OpenRayTracer`PartId->"27506",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27507"->OpenRayTracer`createSphericalLens[0,258.5`,\[Infinity],11.7`,125.`,"N-BK7",OpenRayTracer`PartId->"27507",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"27509"->OpenRayTracer`createSphericalLens[0,155.1`,\[Infinity],24.4`,150,"N-BK7",OpenRayTracer`PartId->"27509",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 12.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\PlanoConvexLambdaDiv20"]]]*)edmundOpticsFusedSilicaLambdaTwenthiethPlanoConvex=<|"33051"->OpenRayTracer`createSphericalLens[0,22.923076923076923`,\[Infinity],5.84`,25.`,"C79-80",OpenRayTracer`PartId->"33051",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33052"->OpenRayTracer`createSphericalLens[0,34.38509316770186`,\[Infinity],4.43`,25.`,"C79-80",OpenRayTracer`PartId->"33052",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33053"->OpenRayTracer`createSphericalLens[0,45.848`,\[Infinity],3.79`,25.`,"C79-80",OpenRayTracer`PartId->"33053",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33054"->OpenRayTracer`createSphericalLens[0,68.78899082568807`,\[Infinity],3.18`,25.`,"C79-80",OpenRayTracer`PartId->"33054",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 12.02.2020 from EdmundOptics website,there were differences between type offused silica Corning substrate specified in the zemax files and at the website specifications.Based on information from mail obtained from Edmund representative,it appears that the spcieficiations at the website are correct.Accordingly,I changed the corning type manually,Association[Join[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\UltraThinFusedSilicaPlanoConvex"]],Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\UltraThinFusedSilicaPlanoConvex\\Unicode"]]]]*)edmundOpticsFusedSilicaUltraThinLaserGradePlanoConvex=<|"11692"->OpenRayTracer`createSphericalLens[0,22.95`,\[Infinity],1.5`,12.7`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11692",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11693"->OpenRayTracer`createSphericalLens[0,34.425`,\[Infinity],1.5`,12.7`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11693",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11694"->OpenRayTracer`createSphericalLens[0,45.9`,\[Infinity],1.5`,12.7`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11694",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11695"->OpenRayTracer`createSphericalLens[0,57.38`,\[Infinity],1.5`,12.7`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11695",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11696"->OpenRayTracer`createSphericalLens[0,68.85`,\[Infinity],1.5`,12.7`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11696",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11697"->OpenRayTracer`createSphericalLens[0,91.8`,\[Infinity],1.5`,12.7`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11697",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11698"->OpenRayTracer`createSphericalLens[0,114.75`,\[Infinity],1.5`,12.7`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11698",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11699"->OpenRayTracer`createSphericalLens[0,137.7`,\[Infinity],1.5`,12.7`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11699",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11700"->OpenRayTracer`createSphericalLens[0,91.8`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11700",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11701"->OpenRayTracer`createSphericalLens[0,114.75`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11701",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11702"->OpenRayTracer`createSphericalLens[0,137.7`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11702",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11703"->OpenRayTracer`createSphericalLens[0,160.65`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11703",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11704"->OpenRayTracer`createSphericalLens[0,183.6`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11704",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11705"->OpenRayTracer`createSphericalLens[0,206.55`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11705",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11706"->OpenRayTracer`createSphericalLens[0,229.5`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11706",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11707"->OpenRayTracer`createSphericalLens[0,344.25`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11707",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11708"->OpenRayTracer`createSphericalLens[0,459.`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11708",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11709"->OpenRayTracer`createSphericalLens[0,688.5`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11709",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11710"->OpenRayTracer`createSphericalLens[0,918.`,\[Infinity],1.6`,25.4`,(*changed manually*)"C79-79",OpenRayTracer`PartId->"11710",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11711"->OpenRayTracer`createSphericalLens[0,22.95`,\[Infinity],1.5`,12.7`,"C79-80",OpenRayTracer`PartId->"11711",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11712"->OpenRayTracer`createSphericalLens[0,34.425`,\[Infinity],1.5`,12.7`,"C79-80",OpenRayTracer`PartId->"11712",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11713"->OpenRayTracer`createSphericalLens[0,45.9`,\[Infinity],1.5`,12.7`,"C79-80",OpenRayTracer`PartId->"11713",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11714"->OpenRayTracer`createSphericalLens[0,57.38`,\[Infinity],1.5`,12.7`,"C79-80",OpenRayTracer`PartId->"11714",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11715"->OpenRayTracer`createSphericalLens[0,68.85`,\[Infinity],1.5`,12.7`,"C79-80",OpenRayTracer`PartId->"11715",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11716"->OpenRayTracer`createSphericalLens[0,91.8`,\[Infinity],1.5`,12.7`,"C79-80",OpenRayTracer`PartId->"11716",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11717"->OpenRayTracer`createSphericalLens[0,114.75`,\[Infinity],1.5`,12.7`,"C79-80",OpenRayTracer`PartId->"11717",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11718"->OpenRayTracer`createSphericalLens[0,137.7`,\[Infinity],1.5`,12.7`,"C79-80",OpenRayTracer`PartId->"11718",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11719"->OpenRayTracer`createSphericalLens[0,91.8`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11719",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11720"->OpenRayTracer`createSphericalLens[0,114.75`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11720",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11721"->OpenRayTracer`createSphericalLens[0,137.7`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11721",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11722"->OpenRayTracer`createSphericalLens[0,160.65`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11722",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11723"->OpenRayTracer`createSphericalLens[0,183.6`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11723",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11724"->OpenRayTracer`createSphericalLens[0,206.55`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11724",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11725"->OpenRayTracer`createSphericalLens[0,229.5`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11725",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11726"->OpenRayTracer`createSphericalLens[0,344.25`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11726",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11727"->OpenRayTracer`createSphericalLens[0,459.`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11727",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11728"->OpenRayTracer`createSphericalLens[0,688.5`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11728",OpenRayTracer`GlassCatalogueData->{"CORNING"}],"11729"->OpenRayTracer`createSphericalLens[0,918.`,\[Infinity],1.6`,25.4`,"C79-80",OpenRayTracer`PartId->"11729",OpenRayTracer`GlassCatalogueData->{"CORNING"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 16.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\LaserGradePlanoConvexLensesUncoated"]]]*)edmundOpticsFusedSilicaLaserGradePlanoConvex=<|"38624"->OpenRayTracer`createSphericalLens[0,16.662921348314608`,\[Infinity],9.53`,25.`,"C79-80",OpenRayTracer`PartId->"38624",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38625"->OpenRayTracer`createSphericalLens[0,23.80392156862745`,\[Infinity],6.35`,25.`,"C79-80",OpenRayTracer`PartId->"38625",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38626"->OpenRayTracer`createSphericalLens[0,35.707070707070706`,\[Infinity],5,25.`,"C79-80",OpenRayTracer`PartId->"38626",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38627"->OpenRayTracer`createSphericalLens[0,47.60909090909091`,\[Infinity],4,25.`,"C79-80",OpenRayTracer`PartId->"38627",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38628"->OpenRayTracer`createSphericalLens[0,71.41304347826087`,\[Infinity],4,25.`,"C79-80",OpenRayTracer`PartId->"38628",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38629"->OpenRayTracer`createSphericalLens[0,95.21794871794872`,\[Infinity],4,25.`,"C79-80",OpenRayTracer`PartId->"38629",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38630"->OpenRayTracer`createSphericalLens[0,119.02197802197803`,\[Infinity],4,25.`,"C79-80",OpenRayTracer`PartId->"38630",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38631"->OpenRayTracer`createSphericalLens[0,142.82692307692307`,\[Infinity],4,25.`,"C79-80",OpenRayTracer`PartId->"38631",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38632"->OpenRayTracer`createSphericalLens[0,238.04395604395606`,\[Infinity],4,25.`,"C79-80",OpenRayTracer`PartId->"38632",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38633"->OpenRayTracer`createSphericalLens[0,476.0890410958904`,\[Infinity],4,25.`,"C79-80",OpenRayTracer`PartId->"38633",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38634"->OpenRayTracer`createSphericalLens[0,11.901960784313726`,\[Infinity],5,12.7`,"C79-80",OpenRayTracer`PartId->"38634",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38635"->OpenRayTracer`createSphericalLens[0,16.662921348314608`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38635",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38636"->OpenRayTracer`createSphericalLens[0,23.80392156862745`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38636",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38637"->OpenRayTracer`createSphericalLens[0,35.707070707070706`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38637",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38638"->OpenRayTracer`createSphericalLens[0,47.60909090909091`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38638",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38639"->OpenRayTracer`createSphericalLens[0,71.41304347826087`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38639",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38640"->OpenRayTracer`createSphericalLens[0,95.21794871794872`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38640",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38641"->OpenRayTracer`createSphericalLens[0,119.02197802197803`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38641",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38643"->OpenRayTracer`createSphericalLens[0,142.82692307692307`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38643",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38644"->OpenRayTracer`createSphericalLens[0,238.04395604395606`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38644",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"38645"->OpenRayTracer`createSphericalLens[0,476.0890410958904`,\[Infinity],4,12.7`,"C79-80",OpenRayTracer`PartId->"38645",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67090"->OpenRayTracer`createSphericalLens[0,8.252032520325203`,\[Infinity],4,12,"C79-80",OpenRayTracer`PartId->"67090",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67091"->OpenRayTracer`createSphericalLens[0,11.462068965517242`,\[Infinity],3.92`,12,"C79-80",OpenRayTracer`PartId->"67091",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67092"->OpenRayTracer`createSphericalLens[0,16.505050505050505`,\[Infinity],2.5`,12,"C79-80",OpenRayTracer`PartId->"67092",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67093"->OpenRayTracer`createSphericalLens[0,22.923076923076923`,\[Infinity],2.9`,12,"C79-80",OpenRayTracer`PartId->"67093",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67094"->OpenRayTracer`createSphericalLens[0,17.42105263157895`,\[Infinity],7.25`,25.`,"C79-80",OpenRayTracer`PartId->"67094",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67095"->OpenRayTracer`createSphericalLens[0,22.923076923076923`,\[Infinity],5.84`,25.`,"C79-80",OpenRayTracer`PartId->"67095",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67096"->OpenRayTracer`createSphericalLens[0,34.38509316770186`,\[Infinity],4.43`,25.`,"C79-80",OpenRayTracer`PartId->"67096",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67097"->OpenRayTracer`createSphericalLens[0,45.848`,\[Infinity],3.79`,25.`,"C79-80",OpenRayTracer`PartId->"67097",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67098"->OpenRayTracer`createSphericalLens[0,68.78899082568807`,\[Infinity],3.18`,25.`,"C79-80",OpenRayTracer`PartId->"67098",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67099"->OpenRayTracer`createSphericalLens[0,114.61490683229813`,\[Infinity],2.71`,25.`,"C79-80",OpenRayTracer`PartId->"67099",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"87924"->OpenRayTracer`createSphericalLens[0,5.5`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"87924",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"87925"->OpenRayTracer`createSphericalLens[0,8.25`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"87925",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"87926"->OpenRayTracer`createSphericalLens[0,11.003003003003004`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"87926",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 16.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\FusedSilicaPlanoConcave"]]]*)edmundOpticsFusedSilicaPlanoConcave=<|"45697"->OpenRayTracer`createSphericalLens[0,-4.13`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"45697",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45698"->OpenRayTracer`createSphericalLens[0,-5.5`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"45698",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45699"->OpenRayTracer`createSphericalLens[0,-8.25`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"45699",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45700"->OpenRayTracer`createSphericalLens[0,-6.2`,\[Infinity],2,9.`,"C79-80",OpenRayTracer`PartId->"45700",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45701"->OpenRayTracer`createSphericalLens[0,-8.25`,\[Infinity],2,9.`,"C79-80",OpenRayTracer`PartId->"45701",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45702"->OpenRayTracer`createSphericalLens[0,-12.4`,\[Infinity],2,9.`,"C79-80",OpenRayTracer`PartId->"45702",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48046"->OpenRayTracer`createSphericalLens[0,-11.462068965517242`,\[Infinity],2,12,"C79-80",OpenRayTracer`PartId->"48046",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48047"->OpenRayTracer`createSphericalLens[0,-13.754098360655737`,\[Infinity],2,12,"C79-80",OpenRayTracer`PartId->"48047",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48048"->OpenRayTracer`createSphericalLens[0,-18.338028169014084`,\[Infinity],2,12,"C79-80",OpenRayTracer`PartId->"48048",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48049"->OpenRayTracer`createSphericalLens[0,-22.923076923076923`,\[Infinity],2,12,"C79-80",OpenRayTracer`PartId->"48049",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48315"->OpenRayTracer`createSphericalLens[0,-22.923076923076923`,\[Infinity],2,25.`,"C79-80",OpenRayTracer`PartId->"48315",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48316"->OpenRayTracer`createSphericalLens[0,-34.38509316770186`,\[Infinity],2.5`,25.`,"C79-80",OpenRayTracer`PartId->"48316",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48317"->OpenRayTracer`createSphericalLens[0,-45.84595300261097`,\[Infinity],2.5`,25.`,"C79-80",OpenRayTracer`PartId->"48317",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48318"->OpenRayTracer`createSphericalLens[0,-68.76892430278885`,\[Infinity],2.5`,25.`,"C79-80",OpenRayTracer`PartId->"48318",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48319"->OpenRayTracer`createSphericalLens[0,-91.69194312796209`,\[Infinity],2.5`,25.`,"C79-80",OpenRayTracer`PartId->"48319",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48320"->OpenRayTracer`createSphericalLens[0,-114.61490683229813`,\[Infinity],2.5`,25.`,"C79-80",OpenRayTracer`PartId->"48320",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"85879"->OpenRayTracer`createSphericalLens[0,-45.84595300261097`,\[Infinity],2,12,"C79-80",OpenRayTracer`PartId->"85879",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*zemax files downloaded on 12.02.2020 from EdmundOptics website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\FusedSilicaPlanoConvex"]]]*)edmundOpticsFusedSilicaPlanoConvex=<|"36679"->OpenRayTracer`createSphericalLens[0,9.63`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"36679",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36680"->OpenRayTracer`createSphericalLens[0,16.505050505050505`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"36680",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36681"->OpenRayTracer`createSphericalLens[0,10.084905660377359`,\[Infinity],3,9.`,"C79-80",OpenRayTracer`PartId->"36681",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36682"->OpenRayTracer`createSphericalLens[0,9.17`,\[Infinity],3,10,"C79-80",OpenRayTracer`PartId->"36682",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36683"->OpenRayTracer`createSphericalLens[0,11.46`,\[Infinity],3,10,"C79-80",OpenRayTracer`PartId->"36683",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36684"->OpenRayTracer`createSphericalLens[0,33.01`,\[Infinity],2.5`,12,"C79-80",OpenRayTracer`PartId->"36684",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36685"->OpenRayTracer`createSphericalLens[0,38.51`,\[Infinity],2.5`,12,"C79-80",OpenRayTracer`PartId->"36685",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36686"->OpenRayTracer`createSphericalLens[0,12.38`,\[Infinity],5.5`,18,"C79-80",OpenRayTracer`PartId->"36686",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36687"->OpenRayTracer`createSphericalLens[0,16.505050505050505`,\[Infinity],4.5`,18,"C79-80",OpenRayTracer`PartId->"36687",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36688"->OpenRayTracer`createSphericalLens[0,24.754901960784313`,\[Infinity],3.5`,18,"C79-80",OpenRayTracer`PartId->"36688",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36689"->OpenRayTracer`createSphericalLens[0,16.04494382022472`,\[Infinity],8.5`,25.`,"C79-80",OpenRayTracer`PartId->"36689",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"36690"->OpenRayTracer`createSphericalLens[0,18.34`,\[Infinity],7.5`,25.`,"C79-80",OpenRayTracer`PartId->"36690",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45690"->OpenRayTracer`createSphericalLens[0,4.13`,\[Infinity],2.5`,6,"C79-80",OpenRayTracer`PartId->"45690",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45691"->OpenRayTracer`createSphericalLens[0,5.5`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"45691",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45692"->OpenRayTracer`createSphericalLens[0,8.25`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"45692",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45693"->OpenRayTracer`createSphericalLens[0,6.19`,\[Infinity],3,9.`,"C79-80",OpenRayTracer`PartId->"45693",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45694"->OpenRayTracer`createSphericalLens[0,8.25`,\[Infinity],3,9.`,"C79-80",OpenRayTracer`PartId->"45694",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45695"->OpenRayTracer`createSphericalLens[0,12.38`,\[Infinity],2,9.`,"C79-80",OpenRayTracer`PartId->"45695",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48024"->OpenRayTracer`createSphericalLens[0,11.462068965517242`,\[Infinity],3.92`,12,"C79-80",OpenRayTracer`PartId->"48024",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48025"->OpenRayTracer`createSphericalLens[0,13.754098360655737`,\[Infinity],3.55`,12,"C79-80",OpenRayTracer`PartId->"48025",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48026"->OpenRayTracer`createSphericalLens[0,18.338028169014084`,\[Infinity],3.13`,12,"C79-80",OpenRayTracer`PartId->"48026",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48027"->OpenRayTracer`createSphericalLens[0,22.923076923076923`,\[Infinity],2.9`,12,"C79-80",OpenRayTracer`PartId->"48027",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48028"->OpenRayTracer`createSphericalLens[0,45.848`,\[Infinity],2.44`,12,"C79-80",OpenRayTracer`PartId->"48028",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48273"->OpenRayTracer`createSphericalLens[0,17.42105263157895`,\[Infinity],7.25`,25.`,"C79-80",OpenRayTracer`PartId->"48273",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48274"->OpenRayTracer`createSphericalLens[0,22.923076923076923`,\[Infinity],5.84`,25.`,"C79-80",OpenRayTracer`PartId->"48274",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48275"->OpenRayTracer`createSphericalLens[0,34.38509316770186`,\[Infinity],4.43`,25.`,"C79-80",OpenRayTracer`PartId->"48275",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48276"->OpenRayTracer`createSphericalLens[0,45.848`,\[Infinity],3.79`,25.`,"C79-80",OpenRayTracer`PartId->"48276",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48277"->OpenRayTracer`createSphericalLens[0,57.30808080808081`,\[Infinity],3.42`,25.`,"C79-80",OpenRayTracer`PartId->"48277",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48278"->OpenRayTracer`createSphericalLens[0,68.78899082568807`,\[Infinity],3.18`,25.`,"C79-80",OpenRayTracer`PartId->"48278",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48279"->OpenRayTracer`createSphericalLens[0,80.23109243697479`,\[Infinity],3.01`,25.`,"C79-80",OpenRayTracer`PartId->"48279",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48280"->OpenRayTracer`createSphericalLens[0,91.6919191919192`,\[Infinity],2.88`,25.`,"C79-80",OpenRayTracer`PartId->"48280",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48281"->OpenRayTracer`createSphericalLens[0,114.61490683229813`,\[Infinity],2.71`,25.`,"C79-80",OpenRayTracer`PartId->"48281",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48282"->OpenRayTracer`createSphericalLens[0,183.392`,\[Infinity],2.44`,25.`,"C79-80",OpenRayTracer`PartId->"48282",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48664"->OpenRayTracer`createSphericalLens[0,6.887096774193548`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"48664",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48665"->OpenRayTracer`createSphericalLens[0,11.003003003003004`,\[Infinity],2,6,"C79-80",OpenRayTracer`PartId->"48665",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48666"->OpenRayTracer`createSphericalLens[0,5.502092050209205`,\[Infinity],4,9.`,"C79-80",OpenRayTracer`PartId->"48666",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48667"->OpenRayTracer`createSphericalLens[0,16.505050505050505`,\[Infinity],2,9.`,"C79-80",OpenRayTracer`PartId->"48667",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48668"->OpenRayTracer`createSphericalLens[0,8.252032520325203`,\[Infinity],4,12,"C79-80",OpenRayTracer`PartId->"48668",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48669"->OpenRayTracer`createSphericalLens[0,16.505050505050505`,\[Infinity],2.5`,12,"C79-80",OpenRayTracer`PartId->"48669",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48670"->OpenRayTracer`createSphericalLens[0,27.50793650793651`,\[Infinity],4.5`,25.`,"C79-80",OpenRayTracer`PartId->"48670",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49959"->OpenRayTracer`createSphericalLens[0,13.75`,\[Infinity],6,20,"C79-80",OpenRayTracer`PartId->"49959",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49960"->OpenRayTracer`createSphericalLens[0,16.05`,\[Infinity],5,20,"C79-80",OpenRayTracer`PartId->"49960",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49961"->OpenRayTracer`createSphericalLens[0,18.34`,\[Infinity],4.5`,20,"C79-80",OpenRayTracer`PartId->"49961",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49962"->OpenRayTracer`createSphericalLens[0,22.92`,\[Infinity],4,20,"C79-80",OpenRayTracer`PartId->"49962",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49963"->OpenRayTracer`createSphericalLens[0,27.51`,\[Infinity],3.5`,20,"C79-80",OpenRayTracer`PartId->"49963",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49964"->OpenRayTracer`createSphericalLens[0,36.68`,\[Infinity],3,20,"C79-80",OpenRayTracer`PartId->"49964",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67223"->OpenRayTracer`createSphericalLens[0,34.38509316770186`,\[Infinity],12.5`,50,"C79-80",OpenRayTracer`PartId->"67223",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67224"->OpenRayTracer`createSphericalLens[0,45.85`,\[Infinity],9,50,"C79-80",OpenRayTracer`PartId->"67224",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67225"->OpenRayTracer`createSphericalLens[0,68.77`,\[Infinity],7,50,"C79-80",OpenRayTracer`PartId->"67225",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67226"->OpenRayTracer`createSphericalLens[0,91.69`,\[Infinity],7,50,"C79-80",OpenRayTracer`PartId->"67226",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67227"->OpenRayTracer`createSphericalLens[0,114.62`,\[Infinity],7,50,"C79-80",OpenRayTracer`PartId->"67227",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84277"->OpenRayTracer`createSphericalLens[0,11.462068965517242`,\[Infinity],4.25`,15.`,"C79-80",OpenRayTracer`PartId->"84277",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84278"->OpenRayTracer`createSphericalLens[0,13.754098360655737`,\[Infinity],3.6`,15.`,"C79-80",OpenRayTracer`PartId->"84278",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84279"->OpenRayTracer`createSphericalLens[0,20.360902255639097`,\[Infinity],3,15.`,"C79-80",OpenRayTracer`PartId->"84279",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84280"->OpenRayTracer`createSphericalLens[0,27.50793650793651`,\[Infinity],3,15.`,"C79-80",OpenRayTracer`PartId->"84280",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84281"->OpenRayTracer`createSphericalLens[0,34.38509316770186`,\[Infinity],3,15.`,"C79-80",OpenRayTracer`PartId->"84281",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84282"->OpenRayTracer`createSphericalLens[0,22.923076923076923`,\[Infinity],7,30,"C79-80",OpenRayTracer`PartId->"84282",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84283"->OpenRayTracer`createSphericalLens[0,27.50793650793651`,\[Infinity],6,30,"C79-80",OpenRayTracer`PartId->"84283",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84284"->OpenRayTracer`createSphericalLens[0,41.26190476190476`,\[Infinity],5,30,"C79-80",OpenRayTracer`PartId->"84284",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84285"->OpenRayTracer`createSphericalLens[0,55.016`,\[Infinity],5,30,"C79-80",OpenRayTracer`PartId->"84285",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"84286"->OpenRayTracer`createSphericalLens[0,68.77`,\[Infinity],5,30,"C79-80",OpenRayTracer`PartId->"84286",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"89411"->OpenRayTracer`createSphericalLens[0,4.584905660377358`,\[Infinity],2,5.`,"C79-80",OpenRayTracer`PartId->"89411",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"89412"->OpenRayTracer`createSphericalLens[0,6.875`,\[Infinity],1.7`,5.`,"C79-80",OpenRayTracer`PartId->"89412",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"89413"->OpenRayTracer`createSphericalLens[0,9.17`,\[Infinity],1.5`,5.`,"C79-80",OpenRayTracer`PartId->"89413",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"89414"->OpenRayTracer`createSphericalLens[0,11.46`,\[Infinity],1.5`,5.`,"C79-80",OpenRayTracer`PartId->"89414",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(* ::Input::Initialization:: *)
(*Association[Join[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\PlanoConvexLenses"]],Map[readEdmundOpticsZemaxSingletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\PlanoConvexLenses\\ASCII"]]]]*)edmundOpticsPlanoConvex=<|"32000"->OpenRayTracer`createSphericalLens[0,18.62`,\[Infinity],3.57`,18,"N-BK7",OpenRayTracer`PartId->"32000",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32001"->OpenRayTracer`createSphericalLens[0,18.16`,\[Infinity],3.8`,18,"N-SF5",OpenRayTracer`PartId->"32001",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32003"->OpenRayTracer`createSphericalLens[0,15.52`,\[Infinity],3.18`,15.`,"N-BK7",OpenRayTracer`PartId->"32003",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32004"->OpenRayTracer`createSphericalLens[0,15.12`,\[Infinity],3.35`,15.`,"N-SF5",OpenRayTracer`PartId->"32004",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32006"->OpenRayTracer`createSphericalLens[0,12.11`,\[Infinity],3,12,"N-SF5",OpenRayTracer`PartId->"32006",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32008"->OpenRayTracer`createSphericalLens[0,9.32`,\[Infinity],2.41`,9.`,"N-BK7",OpenRayTracer`PartId->"32008",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32009"->OpenRayTracer`createSphericalLens[0,9.09`,\[Infinity],2.6`,9.`,"N-SF5",OpenRayTracer`PartId->"32009",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32011"->OpenRayTracer`createSphericalLens[0,12.42`,\[Infinity],2.8`,12,"N-BK7",OpenRayTracer`PartId->"32011",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32469"->OpenRayTracer`createSphericalLens[0,6.05`,\[Infinity],1.75`,6,"N-SF5",OpenRayTracer`PartId->"32469",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32471"->OpenRayTracer`createSphericalLens[0,6.2`,\[Infinity],1.6`,6,"N-BK7",OpenRayTracer`PartId->"32471",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32473"->OpenRayTracer`createSphericalLens[0,9.3`,\[Infinity],1.42`,6,"N-BK7",OpenRayTracer`PartId->"32473",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32475"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],4.3`,20,"N-BK7",OpenRayTracer`PartId->"32475",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32477"->OpenRayTracer`createSphericalLens[0,25.84`,\[Infinity],4.9`,25.`,"N-BK7",OpenRayTracer`PartId->"32477",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32479"->OpenRayTracer`createSphericalLens[0,38.76`,\[Infinity],4.5`,25.`,"N-BK7",OpenRayTracer`PartId->"32479",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32481"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],4.3`,25.`,"N-BK7",OpenRayTracer`PartId->"32481",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32483"->OpenRayTracer`createSphericalLens[0,33.63`,\[Infinity],6.35`,30,"N-SF5",OpenRayTracer`PartId->"32483",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32485"->OpenRayTracer`createSphericalLens[0,38.76`,\[Infinity],6.1`,30,"N-BK7",OpenRayTracer`PartId->"32485",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32487"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],6,30,"N-BK7",OpenRayTracer`PartId->"32487",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32848"->OpenRayTracer`createSphericalLens[0,18.61`,\[Infinity],1.6`,6,"N-BK7",OpenRayTracer`PartId->"32848",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32850"->OpenRayTracer`createSphericalLens[0,37.22`,\[Infinity],1.6`,6,"N-BK7",OpenRayTracer`PartId->"32850",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32852"->OpenRayTracer`createSphericalLens[0,24.82`,\[Infinity],2.5`,12,"N-BK7",OpenRayTracer`PartId->"32852",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32854"->OpenRayTracer`createSphericalLens[0,31.02`,\[Infinity],2.5`,12,"N-BK7",OpenRayTracer`PartId->"32854",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32856"->OpenRayTracer`createSphericalLens[0,43.43`,\[Infinity],2.5`,12,"N-BK7",OpenRayTracer`PartId->"32856",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32859"->OpenRayTracer`createSphericalLens[0,37.22`,\[Infinity],3,18,"N-BK7",OpenRayTracer`PartId->"32859",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32861"->OpenRayTracer`createSphericalLens[0,64.62`,\[Infinity],3.5`,25.`,"N-BK7",OpenRayTracer`PartId->"32861",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32863"->OpenRayTracer`createSphericalLens[0,77.55`,\[Infinity],3.5`,25.`,"N-BK7",OpenRayTracer`PartId->"32863",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32865"->OpenRayTracer`createSphericalLens[0,90.47`,\[Infinity],3.5`,25.`,"N-BK7",OpenRayTracer`PartId->"32865",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32936"->OpenRayTracer`createSphericalLens[0,36.18`,\[Infinity],3,20,"N-BK7",OpenRayTracer`PartId->"32936",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32952"->OpenRayTracer`createSphericalLens[0,3.1`,\[Infinity],1.8`,3.`,"N-BK7",OpenRayTracer`PartId->"32952",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32954"->OpenRayTracer`createSphericalLens[0,4.65`,\[Infinity],1.5`,3.`,"N-BK7",OpenRayTracer`PartId->"32954",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32956"->OpenRayTracer`createSphericalLens[0,12.4`,\[Infinity],1.5`,6,"N-BK7",OpenRayTracer`PartId->"32956",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32958"->OpenRayTracer`createSphericalLens[0,13.95`,\[Infinity],2.5`,9.`,"N-BK7",OpenRayTracer`PartId->"32958",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32960"->OpenRayTracer`createSphericalLens[0,20.67`,\[Infinity],4.9`,20,"N-BK7",OpenRayTracer`PartId->"32960",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32962"->OpenRayTracer`createSphericalLens[0,31.01`,\[Infinity],4.5`,20,"N-BK7",OpenRayTracer`PartId->"32962",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32970"->OpenRayTracer`createSphericalLens[0,39.24`,\[Infinity],12,50,"N-SF11",OpenRayTracer`PartId->"32970",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32972"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],10,50,"N-BK7",OpenRayTracer`PartId->"32972",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"32974"->OpenRayTracer`createSphericalLens[0,77.52`,\[Infinity],9,50,"N-BK7",OpenRayTracer`PartId->"32974",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33352"->OpenRayTracer`createSphericalLens[0,18.604938271604937`,\[Infinity],3,12,"N-BK7",OpenRayTracer`PartId->"33352",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33358"->OpenRayTracer`createSphericalLens[0,103.36`,\[Infinity],3.2`,25.`,"N-BK7",OpenRayTracer`PartId->"33358",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33364"->OpenRayTracer`createSphericalLens[0,206.72`,\[Infinity],3.2`,25.`,"N-BK7",OpenRayTracer`PartId->"33364",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33370"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],7,40,"N-BK7",OpenRayTracer`PartId->"33370",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33376"->OpenRayTracer`createSphericalLens[0,103.36`,\[Infinity],5,40,"N-BK7",OpenRayTracer`PartId->"33376",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"33382"->OpenRayTracer`createSphericalLens[0,41.344086021505376`,\[Infinity],8,40,"N-BK7",OpenRayTracer`PartId->"33382",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37775"->OpenRayTracer`createSphericalLens[0,5.17`,\[Infinity],1.8`,6,"N-BK7",OpenRayTracer`PartId->"37775",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37776"->OpenRayTracer`createSphericalLens[0,10.335078534031414`,\[Infinity],1.45`,6,"N-BK7",OpenRayTracer`PartId->"37776",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37777"->OpenRayTracer`createSphericalLens[0,12.92`,\[Infinity],1.45`,6,"N-BK7",OpenRayTracer`PartId->"37777",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37778"->OpenRayTracer`createSphericalLens[0,7.85`,\[Infinity],2.7`,9.`,"N-SF11",OpenRayTracer`PartId->"37778",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37779"->OpenRayTracer`createSphericalLens[0,10.335078534031414`,\[Infinity],2.2`,9.`,"N-BK7",OpenRayTracer`PartId->"37779",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37780"->OpenRayTracer`createSphericalLens[0,12.92`,\[Infinity],2,9.`,"N-BK7",OpenRayTracer`PartId->"37780",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37781"->OpenRayTracer`createSphericalLens[0,9.81`,\[Infinity],2.7`,10,"N-SF11",OpenRayTracer`PartId->"37781",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37782"->OpenRayTracer`createSphericalLens[0,9.044943820224718`,\[Infinity],2.8`,10,"N-BK7",OpenRayTracer`PartId->"37782",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37783"->OpenRayTracer`createSphericalLens[0,12.92`,\[Infinity],2.7`,12,"N-BK7",OpenRayTracer`PartId->"37783",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37784"->OpenRayTracer`createSphericalLens[0,25.84`,\[Infinity],1.9`,12,"N-BK7",OpenRayTracer`PartId->"37784",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37785"->OpenRayTracer`createSphericalLens[0,9.81`,\[Infinity],3.75`,12.5`,"N-SF11",OpenRayTracer`PartId->"37785",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37786"->OpenRayTracer`createSphericalLens[0,11.77`,\[Infinity],3.1`,12.5`,"N-SF11",OpenRayTracer`PartId->"37786",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37787"->OpenRayTracer`createSphericalLens[0,9.044943820224718`,\[Infinity],3.8`,12.5`,"N-BK7",OpenRayTracer`PartId->"37787",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37788"->OpenRayTracer`createSphericalLens[0,10.335078534031414`,\[Infinity],3.5`,12.5`,"N-BK7",OpenRayTracer`PartId->"37788",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37789"->OpenRayTracer`createSphericalLens[0,12.92`,\[Infinity],3.1`,12.5`,"N-BK7",OpenRayTracer`PartId->"37789",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37790"->OpenRayTracer`createSphericalLens[0,15.505050505050505`,\[Infinity],2.7`,12.5`,"N-BK7",OpenRayTracer`PartId->"37790",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37791"->OpenRayTracer`createSphericalLens[0,18.09`,\[Infinity],2.4`,12.5`,"N-BK7",OpenRayTracer`PartId->"37791",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37792"->OpenRayTracer`createSphericalLens[0,20.67`,\[Infinity],2.2`,12.5`,"N-BK7",OpenRayTracer`PartId->"37792",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37793"->OpenRayTracer`createSphericalLens[0,23.254901960784313`,\[Infinity],2,12.5`,"N-BK7",OpenRayTracer`PartId->"37793",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37796"->OpenRayTracer`createSphericalLens[0,25.84`,\[Infinity],2,12.5`,"N-BK7",OpenRayTracer`PartId->"37796",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37798"->OpenRayTracer`createSphericalLens[0,38.76`,\[Infinity],1.7`,12.5`,"N-BK7",OpenRayTracer`PartId->"37798",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37800"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],1.6`,12.5`,"N-BK7",OpenRayTracer`PartId->"37800",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37801"->OpenRayTracer`createSphericalLens[0,10.335078534031414`,\[Infinity],3.8`,12.7`,"N-BK7",OpenRayTracer`PartId->"37801",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37802"->OpenRayTracer`createSphericalLens[0,15.505050505050505`,\[Infinity],2.7`,12.7`,"N-BK7",OpenRayTracer`PartId->"37802",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37803"->OpenRayTracer`createSphericalLens[0,20.67`,\[Infinity],2.3`,12.7`,"N-BK7",OpenRayTracer`PartId->"37803",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37804"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],1.7`,12.7`,"N-BK7",OpenRayTracer`PartId->"37804",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37806"->OpenRayTracer`createSphericalLens[0,12.92`,\[Infinity],3.8`,15.`,"N-BK7",OpenRayTracer`PartId->"37806",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37808"->OpenRayTracer`createSphericalLens[0,25.84`,\[Infinity],2.5`,15.`,"N-BK7",OpenRayTracer`PartId->"37808",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37810"->OpenRayTracer`createSphericalLens[0,15.694915254237289`,\[Infinity],4.3`,18,"N-SF11",OpenRayTracer`PartId->"37810",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37811"->OpenRayTracer`createSphericalLens[0,12.92`,\[Infinity],5.3`,18,"N-BK7",OpenRayTracer`PartId->"37811",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37812"->OpenRayTracer`createSphericalLens[0,15.505050505050505`,\[Infinity],4.4`,18,"N-BK7",OpenRayTracer`PartId->"37812",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37813"->OpenRayTracer`createSphericalLens[0,25.84`,\[Infinity],2.9`,18,"N-BK7",OpenRayTracer`PartId->"37813",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37814"->OpenRayTracer`createSphericalLens[0,18.09`,\[Infinity],6.9`,25.4`,"N-BK7",OpenRayTracer`PartId->"37814",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37815"->OpenRayTracer`createSphericalLens[0,20.67`,\[Infinity],5.9`,25.4`,"N-BK7",OpenRayTracer`PartId->"37815",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37816"->OpenRayTracer`createSphericalLens[0,38.76`,\[Infinity],3.5`,25.4`,"N-BK7",OpenRayTracer`PartId->"37816",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37817"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],3,25.4`,"N-BK7",OpenRayTracer`PartId->"37817",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37818"->OpenRayTracer`createSphericalLens[0,64.6`,\[Infinity],3,25.4`,"N-BK7",OpenRayTracer`PartId->"37818",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37821"->OpenRayTracer`createSphericalLens[0,77.52`,\[Infinity],3,25.4`,"N-BK7",OpenRayTracer`PartId->"37821",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37824"->OpenRayTracer`createSphericalLens[0,27.464912280701753`,\[Infinity],8,35.`,"N-SF11",OpenRayTracer`PartId->"37824",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37825"->OpenRayTracer`createSphericalLens[0,25.84`,\[Infinity],8.5`,35.`,"N-BK7",OpenRayTracer`PartId->"37825",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37827"->OpenRayTracer`createSphericalLens[0,36.175`,\[Infinity],6,35.`,"N-BK7",OpenRayTracer`PartId->"37827",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"37828"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],4.5`,35.`,"N-BK7",OpenRayTracer`PartId->"37828",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"43394"->OpenRayTracer`createSphericalLens[0,0.85`,\[Infinity],0.8`,1.5`,"N-LASF9",OpenRayTracer`PartId->"43394",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"43395"->OpenRayTracer`createSphericalLens[0,1.28`,\[Infinity],0.8`,1.5`,"N-LASF9",OpenRayTracer`PartId->"43395",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"43396"->OpenRayTracer`createSphericalLens[0,1.28`,\[Infinity],0.8`,2,"N-LASF9",OpenRayTracer`PartId->"43396",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"43397"->OpenRayTracer`createSphericalLens[0,1.7`,\[Infinity],0.8`,2,"N-LASF9",OpenRayTracer`PartId->"43397",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"43398"->OpenRayTracer`createSphericalLens[0,1.7`,\[Infinity],0.8`,2.5`,"N-LASF9",OpenRayTracer`PartId->"43398",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"43399"->OpenRayTracer`createSphericalLens[0,2.12`,\[Infinity],0.8`,2.5`,"N-LASF9",OpenRayTracer`PartId->"43399",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45077"->OpenRayTracer`createSphericalLens[0,4.71`,\[Infinity],2.5`,6,"N-SF11",OpenRayTracer`PartId->"45077",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45079"->OpenRayTracer`createSphericalLens[0,7.75`,\[Infinity],2,6,"N-BK7",OpenRayTracer`PartId->"45079",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45081"->OpenRayTracer`createSphericalLens[0,7.06`,\[Infinity],3,9.`,"N-SF11",OpenRayTracer`PartId->"45081",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45083"->OpenRayTracer`createSphericalLens[0,9.42`,\[Infinity],4,12,"N-SF11",OpenRayTracer`PartId->"45083",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45085"->OpenRayTracer`createSphericalLens[0,15.5`,\[Infinity],3,12,"N-BK7",OpenRayTracer`PartId->"45085",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45093"->OpenRayTracer`createSphericalLens[0,10.09`,\[Infinity],5.5`,15.`,"N-SF5",OpenRayTracer`PartId->"45093",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45095"->OpenRayTracer`createSphericalLens[0,20.67`,\[Infinity],3,15.`,"N-BK7",OpenRayTracer`PartId->"45095",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45097INK"->OpenRayTracer`createSphericalLens[0,16.82`,\[Infinity],8,25.`,"N-SF5",OpenRayTracer`PartId->"45097INK",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45097"->OpenRayTracer`createSphericalLens[0,16.82`,\[Infinity],8,25.`,"N-SF5",OpenRayTracer`PartId->"45097",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45117"->OpenRayTracer`createSphericalLens[0,2.35`,\[Infinity],2,3.`,"N-SF11",OpenRayTracer`PartId->"45117",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45119"->OpenRayTracer`createSphericalLens[0,15.5`,\[Infinity],1.3`,6,"N-BK7",OpenRayTracer`PartId->"45119",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45121"->OpenRayTracer`createSphericalLens[0,18.61`,\[Infinity],2.3`,9.`,"N-BK7",OpenRayTracer`PartId->"45121",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45123"->OpenRayTracer`createSphericalLens[0,21.71`,\[Infinity],2.7`,12,"N-BK7",OpenRayTracer`PartId->"45123",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45125"->OpenRayTracer`createSphericalLens[0,37.21`,\[Infinity],2.4`,12,"N-BK7",OpenRayTracer`PartId->"45125",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45127"->OpenRayTracer`createSphericalLens[0,31.01`,\[Infinity],4.7`,25.`,"N-BK7",OpenRayTracer`PartId->"45127",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45141"->OpenRayTracer`createSphericalLens[0,3.62`,\[Infinity],1.8`,3.`,"N-LASF44",OpenRayTracer`PartId->"45141",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45143"->OpenRayTracer`createSphericalLens[0,3.61`,\[Infinity],2.59`,4.5`,"N-LASF44",OpenRayTracer`PartId->"45143",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45145"->OpenRayTracer`createSphericalLens[0,18.11`,\[Infinity],7.01`,25.`,"N-BK7",OpenRayTracer`PartId->"45145",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45147"->OpenRayTracer`createSphericalLens[0,4.65`,\[Infinity],2.38`,4.5`,"N-BK7",OpenRayTracer`PartId->"45147",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45149"->OpenRayTracer`createSphericalLens[0,31.03`,\[Infinity],9.31`,40,"N-BK7",OpenRayTracer`PartId->"45149",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45151"->OpenRayTracer`createSphericalLens[0,103.36`,\[Infinity],5.07`,50,"N-BK7",OpenRayTracer`PartId->"45151",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45153"->OpenRayTracer`createSphericalLens[0,129.21`,\[Infinity],4.94`,50,"N-BK7",OpenRayTracer`PartId->"45153",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45223"->OpenRayTracer`createSphericalLens[0,3.21`,\[Infinity],1.7`,4,"N-LASF44",OpenRayTracer`PartId->"45223",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45225"->OpenRayTracer`createSphericalLens[0,5.36`,\[Infinity],1.39`,4,"N-BAF10",OpenRayTracer`PartId->"45225",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45227"->OpenRayTracer`createSphericalLens[0,4.02`,\[Infinity],1.87`,5.`,"N-LASF44",OpenRayTracer`PartId->"45227",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45229"->OpenRayTracer`createSphericalLens[0,5.17`,\[Infinity],1.64`,5.`,"N-BK7",OpenRayTracer`PartId->"45229",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45231"->OpenRayTracer`createSphericalLens[0,10.85`,\[Infinity],1.42`,6,"N-BK7",OpenRayTracer`PartId->"45231",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45233"->OpenRayTracer`createSphericalLens[0,24.81`,\[Infinity],1.6`,6,"N-BK7",OpenRayTracer`PartId->"45233",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45235"->OpenRayTracer`createSphericalLens[0,11.37`,\[Infinity],1.93`,9.`,"N-BK7",OpenRayTracer`PartId->"45235",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45237"->OpenRayTracer`createSphericalLens[0,15.7`,\[Infinity],4.6`,20,"N-SF11",OpenRayTracer`PartId->"45237",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45239"->OpenRayTracer`createSphericalLens[0,20.1`,\[Infinity],4.5`,20,"N-BAF10",OpenRayTracer`PartId->"45239",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45241"->OpenRayTracer`createSphericalLens[0,31.01`,\[Infinity],6,30,"N-BK7",OpenRayTracer`PartId->"45241",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45243"->OpenRayTracer`createSphericalLens[0,62.02`,\[Infinity],6,30,"N-BK7",OpenRayTracer`PartId->"45243",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45245"->OpenRayTracer`createSphericalLens[0,38.76`,\[Infinity],11,50,"N-BK7",OpenRayTracer`PartId->"45245",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45247"->OpenRayTracer`createSphericalLens[0,64.6`,\[Infinity],10,50,"N-BK7",OpenRayTracer`PartId->"45247",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45249"->OpenRayTracer`createSphericalLens[0,90.44`,\[Infinity],9,50,"N-BK7",OpenRayTracer`PartId->"45249",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45259INK"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],3,15.`,"N-BK7",OpenRayTracer`PartId->"45259INK",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45259"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],3,15.`,"N-BK7",OpenRayTracer`PartId->"45259",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45272"->OpenRayTracer`createSphericalLens[0,6.2`,\[Infinity],1.08`,3.`,"N-BK7",OpenRayTracer`PartId->"45272",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45274"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],2.5`,12,"N-BK7",OpenRayTracer`PartId->"45274",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45276"->OpenRayTracer`createSphericalLens[0,77.52`,\[Infinity],3,20,"N-BK7",OpenRayTracer`PartId->"45276",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45278"->OpenRayTracer`createSphericalLens[0,20.67`,\[Infinity],5.6`,25.`,"N-BK7",OpenRayTracer`PartId->"45278",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45280"->OpenRayTracer`createSphericalLens[0,258.4`,\[Infinity],3.2`,25.`,"N-BK7",OpenRayTracer`PartId->"45280",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45282"->OpenRayTracer`createSphericalLens[0,387.6`,\[Infinity],3.2`,25.`,"N-BK7",OpenRayTracer`PartId->"45282",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45284"->OpenRayTracer`createSphericalLens[0,155.04`,\[Infinity],5,40,"N-BK7",OpenRayTracer`PartId->"45284",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45286"->OpenRayTracer`createSphericalLens[0,206.72`,\[Infinity],5,40,"N-BK7",OpenRayTracer`PartId->"45286",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45288"->OpenRayTracer`createSphericalLens[0,258.4`,\[Infinity],5,50,"N-BK7",OpenRayTracer`PartId->"45288",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45302"->OpenRayTracer`createSphericalLens[0,7.75`,\[Infinity],4.5`,12,"N-BK7",OpenRayTracer`PartId->"45302",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45304"->OpenRayTracer`createSphericalLens[0,27.91`,\[Infinity],3,12,"N-BK7",OpenRayTracer`PartId->"45304",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45306"->OpenRayTracer`createSphericalLens[0,27.91`,\[Infinity],3,18,"N-BK7",OpenRayTracer`PartId->"45306",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45356"->OpenRayTracer`createSphericalLens[0,7.75`,\[Infinity],1.6`,5.`,"N-BK7",OpenRayTracer`PartId->"45356",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45358"->OpenRayTracer`createSphericalLens[0,23.26`,\[Infinity],1.54`,9.`,"N-BK7",OpenRayTracer`PartId->"45358",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45360"->OpenRayTracer`createSphericalLens[0,38.76`,\[Infinity],1.83`,15.`,"N-BK7",OpenRayTracer`PartId->"45360",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45362"->OpenRayTracer`createSphericalLens[0,25.84`,\[Infinity],3.23`,20,"N-BK7",OpenRayTracer`PartId->"45362",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45364INK"->OpenRayTracer`createSphericalLens[0,15.5`,\[Infinity],8.06`,25.`,"N-BK7",OpenRayTracer`PartId->"45364INK",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45364"->OpenRayTracer`createSphericalLens[0,15.5`,\[Infinity],8.06`,25.`,"N-BK7",OpenRayTracer`PartId->"45364",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45366"->OpenRayTracer`createSphericalLens[0,43.93`,\[Infinity],4.3`,25.`,"N-BK7",OpenRayTracer`PartId->"45366",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45367"->OpenRayTracer`createSphericalLens[0,38.76`,\[Infinity],32.62`,75.`,"N-BK7",OpenRayTracer`PartId->"45367",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45369"->OpenRayTracer`createSphericalLens[0,77.52`,\[Infinity],11.45`,75.`,"N-BK7",OpenRayTracer`PartId->"45369",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45371"->OpenRayTracer`createSphericalLens[0,103.36`,\[Infinity],8.74`,75.`,"N-BK7",OpenRayTracer`PartId->"45371",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45425"->OpenRayTracer`createSphericalLens[0,7.78`,\[Infinity],1.26`,3.`,"N-BK7",OpenRayTracer`PartId->"45425",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45427"->OpenRayTracer`createSphericalLens[0,3.11`,\[Infinity],2.26`,4,"N-BK7",OpenRayTracer`PartId->"45427",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45429"->OpenRayTracer`createSphericalLens[0,5.19`,\[Infinity],1.64`,4,"N-BK7",OpenRayTracer`PartId->"45429",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45431"->OpenRayTracer`createSphericalLens[0,6.22`,\[Infinity],1.52`,4,"N-BK7",OpenRayTracer`PartId->"45431",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45433"->OpenRayTracer`createSphericalLens[0,6.22`,\[Infinity],1.77`,5.`,"N-BK7",OpenRayTracer`PartId->"45433",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45435"->OpenRayTracer`createSphericalLens[0,31.11`,\[Infinity],2.05`,15.`,"N-BK7",OpenRayTracer`PartId->"45435",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45439"->OpenRayTracer`createSphericalLens[0,41.48`,\[Infinity],2.35`,20,"N-BK7",OpenRayTracer`PartId->"45439",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45588"->OpenRayTracer`createSphericalLens[0,0.51`,\[Infinity],0.8`,1.`,"N-LASF9",OpenRayTracer`PartId->"45588",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45589"->OpenRayTracer`createSphericalLens[0,0.85`,\[Infinity],0.8`,1.`,"N-LASF9",OpenRayTracer`PartId->"45589",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45590"->OpenRayTracer`createSphericalLens[0,2.55`,\[Infinity],0.8`,2.5`,"N-LASF9",OpenRayTracer`PartId->"45590",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48423"->OpenRayTracer`createSphericalLens[0,14.13`,\[Infinity],5,18,"N-SF11",OpenRayTracer`PartId->"48423",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48643"->OpenRayTracer`createSphericalLens[0,3.401098901098901`,\[Infinity],0.8`,2.5`,"N-LASF9",OpenRayTracer`PartId->"48643",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48644"->OpenRayTracer`createSphericalLens[0,4.251082251082251`,\[Infinity],0.8`,2.5`,"N-LASF9",OpenRayTracer`PartId->"48644",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48645"->OpenRayTracer`createSphericalLens[0,3.87603305785124`,\[Infinity],1.5`,3.`,"N-BK7",OpenRayTracer`PartId->"48645",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48652"->OpenRayTracer`createSphericalLens[0,5.885057471264368`,\[Infinity],1.9`,5.`,"N-SF11",OpenRayTracer`PartId->"48652",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48653"->OpenRayTracer`createSphericalLens[0,23.540983606557376`,\[Infinity],6.9`,30,"N-SF11",OpenRayTracer`PartId->"48653",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48654"->OpenRayTracer`createSphericalLens[0,31.38909090909091`,\[Infinity],8.7`,40,"N-SF11",OpenRayTracer`PartId->"48654",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49839"->OpenRayTracer`createSphericalLens[0,9.97`,\[Infinity],4,12.7`,"N-SF11",OpenRayTracer`PartId->"49839",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49840"->OpenRayTracer`createSphericalLens[0,7.75`,\[Infinity],5.25`,12.7`,"N-BK7",OpenRayTracer`PartId->"49840",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49841"->OpenRayTracer`createSphericalLens[0,9.87`,\[Infinity],4,12.7`,"N-BK7",OpenRayTracer`PartId->"49841",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49842"->OpenRayTracer`createSphericalLens[0,13.13`,\[Infinity],3,12.7`,"N-BK7",OpenRayTracer`PartId->"49842",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49843"->OpenRayTracer`createSphericalLens[0,16.43`,\[Infinity],3,12.7`,"N-BK7",OpenRayTracer`PartId->"49843",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49844"->OpenRayTracer`createSphericalLens[0,19.69`,\[Infinity],3,12.7`,"N-BK7",OpenRayTracer`PartId->"49844",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49845"->OpenRayTracer`createSphericalLens[0,23.`,\[Infinity],2.75`,12.7`,"N-BK7",OpenRayTracer`PartId->"49845",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49846"->OpenRayTracer`createSphericalLens[0,26.25`,\[Infinity],2.5`,12.7`,"N-BK7",OpenRayTracer`PartId->"49846",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49847"->OpenRayTracer`createSphericalLens[0,19.93`,\[Infinity],7,25.4`,"N-SF11",OpenRayTracer`PartId->"49847",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49848"->OpenRayTracer`createSphericalLens[0,19.69`,\[Infinity],7,25.4`,"N-BK7",OpenRayTracer`PartId->"49848",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49849"->OpenRayTracer`createSphericalLens[0,26.25`,\[Infinity],5,25.4`,"N-BK7",OpenRayTracer`PartId->"49849",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49850"->OpenRayTracer`createSphericalLens[0,32.82`,\[Infinity],4.75`,25.4`,"N-BK7",OpenRayTracer`PartId->"49850",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49851"->OpenRayTracer`createSphericalLens[0,39.38`,\[Infinity],4,25.4`,"N-BK7",OpenRayTracer`PartId->"49851",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49852"->OpenRayTracer`createSphericalLens[0,45.94`,\[Infinity],4,25.4`,"N-BK7",OpenRayTracer`PartId->"49852",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49853"->OpenRayTracer`createSphericalLens[0,52.51`,\[Infinity],4,25.4`,"N-BK7",OpenRayTracer`PartId->"49853",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49873"->OpenRayTracer`createSphericalLens[0,10.34`,\[Infinity],1.5`,5.`,"N-BK7",OpenRayTracer`PartId->"49873",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49874"->OpenRayTracer`createSphericalLens[0,5.885057471264368`,\[Infinity],2.5`,6,"N-SF11",OpenRayTracer`PartId->"49874",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49875"->OpenRayTracer`createSphericalLens[0,9.42`,\[Infinity],2.5`,9.`,"N-SF11",OpenRayTracer`PartId->"49875",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49876"->OpenRayTracer`createSphericalLens[0,7.75`,\[Infinity],3,9.`,"N-BK7",OpenRayTracer`PartId->"49876",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49877"->OpenRayTracer`createSphericalLens[0,10.335078534031414`,\[Infinity],3.5`,12,"N-BK7",OpenRayTracer`PartId->"49877",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49878"->OpenRayTracer`createSphericalLens[0,12.11`,\[Infinity],4,15.`,"N-SF5",OpenRayTracer`PartId->"49878",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49879"->OpenRayTracer`createSphericalLens[0,23.254901960784313`,\[Infinity],2.5`,15.`,"N-BK7",OpenRayTracer`PartId->"49879",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49880"->OpenRayTracer`createSphericalLens[0,17.66`,\[Infinity],4,18,"N-SF11",OpenRayTracer`PartId->"49880",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49881"->OpenRayTracer`createSphericalLens[0,19.62`,\[Infinity],4,20,"N-SF11",OpenRayTracer`PartId->"49881",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"49882"->OpenRayTracer`createSphericalLens[0,18.09`,\[Infinity],4.5`,20,"N-BK7",OpenRayTracer`PartId->"49882",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63471"->OpenRayTracer`createSphericalLens[0,7.85`,\[Infinity],3.25`,10,"N-SF11",OpenRayTracer`PartId->"63471",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63472"->OpenRayTracer`createSphericalLens[0,11.77`,\[Infinity],3,10,"N-SF11",OpenRayTracer`PartId->"63472",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63473"->OpenRayTracer`createSphericalLens[0,10.34`,\[Infinity],3,10,"N-BK7",OpenRayTracer`PartId->"63473",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63474"->OpenRayTracer`createSphericalLens[0,12.92`,\[Infinity],3,10,"N-BK7",OpenRayTracer`PartId->"63474",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63475"->OpenRayTracer`createSphericalLens[0,15.5`,\[Infinity],3,10,"N-BK7",OpenRayTracer`PartId->"63475",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63476"->OpenRayTracer`createSphericalLens[0,20.67`,\[Infinity],3,10,"N-BK7",OpenRayTracer`PartId->"63476",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63477"->OpenRayTracer`createSphericalLens[0,25.84`,\[Infinity],3,10,"N-BK7",OpenRayTracer`PartId->"63477",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63478"->OpenRayTracer`createSphericalLens[0,38.76`,\[Infinity],3,10,"N-BK7",OpenRayTracer`PartId->"63478",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63479"->OpenRayTracer`createSphericalLens[0,51.68`,\[Infinity],3,10,"N-BK7",OpenRayTracer`PartId->"63479",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63480"->OpenRayTracer`createSphericalLens[0,13.46`,\[Infinity],3.8`,15.`,"N-SF5",OpenRayTracer`PartId->"63480",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63482"->OpenRayTracer`createSphericalLens[0,129.2`,\[Infinity],8,75.`,"N-BK7",OpenRayTracer`PartId->"63482",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"63483"->OpenRayTracer`createSphericalLens[0,258.4`,\[Infinity],8,75.`,"N-BK7",OpenRayTracer`PartId->"63483",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"65252"->OpenRayTracer`createSphericalLens[0,1.275`,\[Infinity],0.8`,1.`,"N-LASF9",OpenRayTracer`PartId->"65252",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"65253"->OpenRayTracer`createSphericalLens[0,1.7`,\[Infinity],0.8`,1.`,"N-LASF9",OpenRayTracer`PartId->"65253",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"65254"->OpenRayTracer`createSphericalLens[0,1.7`,\[Infinity],0.8`,1.5`,"N-LASF9",OpenRayTracer`PartId->"65254",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"65255"->OpenRayTracer`createSphericalLens[0,2.55`,\[Infinity],0.8`,2,"N-LASF9",OpenRayTracer`PartId->"65255",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"65256"->OpenRayTracer`createSphericalLens[0,3.4`,\[Infinity],0.8`,2,"N-LASF9",OpenRayTracer`PartId->"65256",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67146"->OpenRayTracer`createSphericalLens[0,31.01`,\[Infinity],1.6`,6,"N-BK7",OpenRayTracer`PartId->"67146",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67147"->OpenRayTracer`createSphericalLens[0,27.91`,\[Infinity],2,9.`,"N-BK7",OpenRayTracer`PartId->"67147",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67148"->OpenRayTracer`createSphericalLens[0,37.21`,\[Infinity],2,9.`,"N-BK7",OpenRayTracer`PartId->"67148",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67149"->OpenRayTracer`createSphericalLens[0,46.51`,\[Infinity],2,9.`,"N-BK7",OpenRayTracer`PartId->"67149",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67150"->OpenRayTracer`createSphericalLens[0,129.2`,\[Infinity],3,25.`,"N-BK7",OpenRayTracer`PartId->"67150",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67151"->OpenRayTracer`createSphericalLens[0,155.04`,\[Infinity],3,25.`,"N-BK7",OpenRayTracer`PartId->"67151",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"67152"->OpenRayTracer`createSphericalLens[0,31.39`,\[Infinity],5,30,"N-SF11",OpenRayTracer`PartId->"67152",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"87911"->OpenRayTracer`createSphericalLens[0,12.92`,\[Infinity],1.6`,5.`,"N-BK7",OpenRayTracer`PartId->"87911",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"87912"->OpenRayTracer`createSphericalLens[0,15.504065040650406`,\[Infinity],1.6`,5.`,"N-BK7",OpenRayTracer`PartId->"87912",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"14374"->OpenRayTracer`createSphericalLens[0,103.36`,\[Infinity],3.2`,15.`,"N-BK7",OpenRayTracer`PartId->"14374",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}],"14382"->OpenRayTracer`createSphericalLens[0,129.2`,\[Infinity],3,15.`,"N-BK7",OpenRayTracer`PartId->"14382",OpenRayTracer`GlassCatalogueData->{"SCHOTT"}]|>;


(* ::Input::Initialization:: *)
(*zmx files downloaded on 16.06.2020 from Edmund Optics Website,Association[Map[readEdmundOpticsZemaxSingletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\PlanoConcaveLenses"]]]*)edmundOpticsPlanoConcave=<|"45006"->OpenRayTracer`createSphericalLens[0,-4.71`,\[Infinity],1.5`,6,"N-SF11",OpenRayTracer`PartId->"45006",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45008"->OpenRayTracer`createSphericalLens[0,-6.2`,\[Infinity],1.5`,6,"N-BK7",OpenRayTracer`PartId->"45008",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45010"->OpenRayTracer`createSphericalLens[0,-9.3`,\[Infinity],2,6,"N-BK7",OpenRayTracer`PartId->"45010",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45014"->OpenRayTracer`createSphericalLens[0,-9.42`,\[Infinity],2.2`,12,"N-SF11",OpenRayTracer`PartId->"45014",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45016"->OpenRayTracer`createSphericalLens[0,-12.4`,\[Infinity],3.5`,12,"N-BK7",OpenRayTracer`PartId->"45016",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45018"->OpenRayTracer`createSphericalLens[0,-24.81`,\[Infinity],3.5`,12,"N-BK7",OpenRayTracer`PartId->"45018",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45020"->OpenRayTracer`createSphericalLens[0,-23.54`,\[Infinity],3.5`,20,"N-SF11",OpenRayTracer`PartId->"45020",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45022"->OpenRayTracer`createSphericalLens[0,-20.67`,\[Infinity],3.5`,20,"N-BK7",OpenRayTracer`PartId->"45022",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45024"->OpenRayTracer`createSphericalLens[0,-25.84`,\[Infinity],3.5`,20,"N-BK7",OpenRayTracer`PartId->"45024",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45026"->OpenRayTracer`createSphericalLens[0,-51.68`,\[Infinity],3.5`,25.`,"N-BK7",OpenRayTracer`PartId->"45026",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45028"->OpenRayTracer`createSphericalLens[0,-25.84`,\[Infinity],3.5`,25.`,"N-BK7",OpenRayTracer`PartId->"45028",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45030"->OpenRayTracer`createSphericalLens[0,-19.62`,\[Infinity],3.5`,25.`,"N-SF11",OpenRayTracer`PartId->"45030",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45036"->OpenRayTracer`createSphericalLens[0,-51.68`,\[Infinity],5,50,"N-BK7",OpenRayTracer`PartId->"45036",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45038"->OpenRayTracer`createSphericalLens[0,-64.6`,\[Infinity],5,50,"N-BK7",OpenRayTracer`PartId->"45038",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45374"->OpenRayTracer`createSphericalLens[0,-4.71`,\[Infinity],1,3.`,"N-SF11",OpenRayTracer`PartId->"45374",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45376"->OpenRayTracer`createSphericalLens[0,-7.06`,\[Infinity],1,3.`,"N-SF11",OpenRayTracer`PartId->"45376",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45380"->OpenRayTracer`createSphericalLens[0,-7.07`,\[Infinity],2.25`,9.`,"N-SF11",OpenRayTracer`PartId->"45380",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45382"->OpenRayTracer`createSphericalLens[0,-14.12`,\[Infinity],2.25`,9.`,"N-SF11",OpenRayTracer`PartId->"45382",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"45384"->OpenRayTracer`createSphericalLens[0,-21.19`,\[Infinity],3,9.`,"N-SF11",OpenRayTracer`PartId->"45384",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48333"->OpenRayTracer`createSphericalLens[0,-7.06`,\[Infinity],1.5`,6,"N-SF11",OpenRayTracer`PartId->"48333",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48334"->OpenRayTracer`createSphericalLens[0,-9.42`,\[Infinity],2.25`,9.`,"N-SF11",OpenRayTracer`PartId->"48334",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48335"->OpenRayTracer`createSphericalLens[0,-14.12`,\[Infinity],3.5`,12,"N-SF11",OpenRayTracer`PartId->"48335",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48678"->OpenRayTracer`createSphericalLens[0,-7.75206611570248`,\[Infinity],2,6,"N-BK7",OpenRayTracer`PartId->"48678",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48679"->OpenRayTracer`createSphericalLens[0,-11.771084337349398`,\[Infinity],3,12,"N-SF11",OpenRayTracer`PartId->"48679",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}],"48680"->OpenRayTracer`createSphericalLens[0,-27.464912280701753`,\[Infinity],3.5`,25.`,"N-SF11",OpenRayTracer`PartId->"48680",OpenRayTracer`GlassCatalogueData->{"SCHOTT","OHARA","MISC","CORNING","INFRARED"}]|>;


(*zmx files downloaded on 18.06.2020 from Edmund Optics Website,Association[Join[Map[readEdmundOpticsZemaxCementedDoubletSphericalLensFile[#,CharacterEncoding\[Rule]"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\VisibleAchromaticPositiveCementedDoubletLenses\\Unicode"]],Map[readEdmundOpticsZemaxCementedDoubletSphericalLensFile[#]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\VisibleAchromaticPositiveCementedDoubletLenses\\ASCII"]]]]*)
edmundOpticsPositiveCementedDoubletsAchromaticVisible=<|"32299"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "8.63`", ",", 
RowBox[{"-", "5.29`"}], ",", 
RowBox[{"-", "51.17`"}], ",", "3", ",", "0.8`", ",", "6.25`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32299\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32301"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.95`", ",", 
RowBox[{"-", "7.25`"}], ",", 
RowBox[{"-", "277.83`"}], ",", "2.7`", ",", "0.6`", ",", "6.25`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32301\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32303"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.3`", ",", 
RowBox[{"-", "8.71`"}], ",", 
RowBox[{"-", "24.98`"}], ",", "2.7`", ",", "0.9`", ",", "6.25`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32303\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32305"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "15.37`", ",", 
RowBox[{"-", "11.16`"}], ",", 
RowBox[{"-", "32.17`"}], ",", "2.3`", ",", "0.9`", ",", "6.25`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32305\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32307"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "23.14`", ",", 
RowBox[{"-", "14.51`"}], ",", 
RowBox[{"-", "38.86`"}], ",", "2.3`", ",", "0.9`", ",", "6.25`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32307\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32309"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "13.98`", ",", 
RowBox[{"-", "9.35`"}], ",", 
RowBox[{"-", "76.14`"}], ",", "5.25`", ",", "1.1`", ",", "12.5`", ",", "\"\<S-BAH11\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32309\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32311"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "17.27`", ",", 
RowBox[{"-", "10.99`"}], ",", 
RowBox[{"-", "107.03`"}], ",", "5", ",", "1.25`", ",", "12.5`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32311\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32313"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "15.97`", ",", 
RowBox[{"-", "15.`"}], ",", 
RowBox[{"-", "528.23`"}], ",", "4.5`", ",", "1.2`", ",", "12.5`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32313\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32315"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "24.77`", ",", 
RowBox[{"-", "18.01`"}], ",", 
RowBox[{"-", "51.17`"}], ",", "3.73`", ",", "1.33`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32315\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32317"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "30.73`", ",", 
RowBox[{"-", "22.32`"}], ",", 
RowBox[{"-", "64.75`"}], ",", "3.5`", ",", "1.5`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32317\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32319"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "24.47`", ",", 
RowBox[{"-", "16.49`"}], ",", 
RowBox[{"-", "131.65`"}], ",", "11", ",", "2.5`", ",", "25.`", ",", "\"\<S-BAH11\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32319\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32321"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "27.97`", ",", 
RowBox[{"-", "18.85`"}], ",", 
RowBox[{"-", "152.94`"}], ",", "9.5`", ",", "2.5`", ",", "25.`", ",", "\"\<S-BAH11\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32321\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32323"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "34.53`", ",", 
RowBox[{"-", "21.98`"}], ",", 
RowBox[{"-", "214.63`"}], ",", "9", ",", "2.5`", ",", "25.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32323\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32325"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "46.44`", ",", 
RowBox[{"-", "33.77`"}], ",", 
RowBox[{"-", "95.94`"}], ",", "7", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32325\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32327"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "61.47`", ",", 
RowBox[{"-", "44.64`"}], ",", 
RowBox[{"-", "129.94`"}], ",", "6", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32327\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32492"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "76.28`", ",", 
RowBox[{"-", "54.55`"}], ",", 
RowBox[{"-", "162.43`"}], ",", "6", ",", "2.4`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32492\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32494"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "91.37`", ",", 
RowBox[{"-", "66.21`"}], ",", 
RowBox[{"-", "197.71`"}], ",", "5.7`", ",", "2.2`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32494\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32496"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "34.81`", ",", 
RowBox[{"-", "22.12`"}], ",", 
RowBox[{"-", "203.48`"}], ",", "11", ",", "2.2`", ",", "30", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32496\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32498"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "40.51`", ",", 
RowBox[{"-", "38.68`"}], ",", 
RowBox[{"-", "922.04`"}], ",", "8.4`", ",", "3", ",", "30", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32498\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", "\"\<SCHOTT\>\"", "}"}]}]}], "]"}],
HoldForm]\),
"32500"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "61.36`", ",", 
RowBox[{"-", "44.3`"}], ",", 
RowBox[{"-", "128.9`"}], ",", "8.25`", ",", "2.8`", ",", "30", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32500\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32502"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "91.31`", ",", 
RowBox[{"-", "65.57`"}], ",", 
RowBox[{"-", "195.87`"}], ",", "8.1`", ",", "2.6`", ",", "30", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32502\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32720"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "20.72`", ",", 
RowBox[{"-", "13.17`"}], ",", 
RowBox[{"-", "128.44`"}], ",", "6", ",", "1.5`", ",", "15.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32720\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32722"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "27.63`", ",", 
RowBox[{"-", "17.58`"}], ",", 
RowBox[{"-", "171.25`"}], ",", "8", ",", "2", ",", "20", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32722\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32724"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "31.94`", ",", 
RowBox[{"-", "29.95`"}], ",", 
RowBox[{"-", "1029.79`"}], ",", "9", ",", "2.4`", ",", "25.`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32724\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32882"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "46.44`", ",", 
RowBox[{"-", "33.77`"}], ",", 
RowBox[{"-", "95.94`"}], ",", "3", ",", "1.6`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32882\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32884"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "109.16`", ",", 
RowBox[{"-", "79.38`"}], ",", 
RowBox[{"-", "226.03`"}], ",", "6", ",", "3", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32884\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32886"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "96.85`", ",", 
RowBox[{"-", "73.74`"}], ",", 
RowBox[{"-", "241.63`"}], ",", "9.5`", ",", "4", ",", "50", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32886\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32913"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "27.12`", ",", 
RowBox[{"-", "25.66`"}], ",", 
RowBox[{"-", "538.7`"}], ",", "5", ",", "2.3`", ",", "18", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32913\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32915"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "49.67`", ",", 
RowBox[{"-", "34.73`"}], ",", 
RowBox[{"-", "100.11`"}], ",", "5", ",", "2.5`", ",", "18", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32915\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32917"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "124.12`", ",", 
RowBox[{"-", "87.26`"}], ",", 
RowBox[{"-", "253.1`"}], ",", "8.5`", ",", "4", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32917\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32919"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "160.73`", ",", 
RowBox[{"-", "105.64`"}], ",", 
RowBox[{"-", "295.75`"}], ",", "5", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32919\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32921"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "65.22`", ",", 
RowBox[{"-", "62.03`"}], ",", 
RowBox[{"-", "1240.67`"}], ",", "9.6`", ",", "4.2`", ",", "40", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32921\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32923"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "98.66`", ",", 
RowBox[{"-", "70.73`"}], ",", 
RowBox[{"-", "205.72`"}], ",", "8.5`", ",", "4", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32923\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32925"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "124.12`", ",", 
RowBox[{"-", "87.26`"}], ",", 
RowBox[{"-", "253.1`"}], ",", "8.5`", ",", "4", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32925\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32927"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "257.16`", ",", 
RowBox[{"-", "169.03`"}], ",", 
RowBox[{"-", "473.08`"}], ",", "8.5`", ",", "4", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32927\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"32935"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "156.`", ",", 
RowBox[{"-", "108.7`"}], ",", 
RowBox[{"-", "313.64`"}], ",", "8", ",", "4.5`", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32935\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45089"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "4.13`", ",", 
RowBox[{"-", "2.36`"}], ",", 
RowBox[{"-", "21.7`"}], ",", "1.97`", ",", "1.03`", ",", "3.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45089\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45090"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "5.26`", ",", 
RowBox[{"-", "3.98`"}], ",", 
RowBox[{"-", "12.05`"}], ",", "1.47`", ",", "1.03`", ",", "3.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45090\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45091"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.37`", ",", 
RowBox[{"-", "7.38`"}], ",", 
RowBox[{"-", "73.86`"}], ",", "4.32`", ",", "1.15`", ",", "9.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45091\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45092"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "15.36`", ",", 
RowBox[{"-", "12.01`"}], ",", 
RowBox[{"-", "39.56`"}], ",", "3.89`", ",", "1.3`", ",", "9.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45092\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45103"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "26.97`", ",", 
RowBox[{"-", "17.64`"}], ",", 
RowBox[{"-", "205.03`"}], ",", "5.98`", ",", "1.6`", ",", "15.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45103\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45104"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "26.97`", ",", 
RowBox[{"-", "17.64`"}], ",", 
RowBox[{"-", "205.03`"}], ",", "5.98`", ",", "1.6`", ",", "18", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45104\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45105"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "53.76`", ",", 
RowBox[{"-", "36.73`"}], ",", 
RowBox[{"-", "435.85`"}], ",", "12.07`", ",", "4", ",", "40", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45105\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45135"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "18.11`", ",", 
RowBox[{"-", "13.51`"}], ",", 
RowBox[{"-", "39.29`"}], ",", "2.3`", ",", "0.9`", ",", "6.25`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45135\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45136"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "26.82`", ",", 
RowBox[{"-", "20.12`"}], ",", 
RowBox[{"-", "61.38`"}], ",", "3.5`", ",", "1.5`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45136\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45137INK"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "36.51`", ",", 
RowBox[{"-", "26.33`"}], ",", 
RowBox[{"-", "78.35`"}], ",", "3.5`", ",", "1.5`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45137INK\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45137"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "36.51`", ",", 
RowBox[{"-", "26.33`"}], ",", 
RowBox[{"-", "78.35`"}], ",", "3.5`", ",", "1.5`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45137\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45138"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "26.81`", ",", 
RowBox[{"-", "23.82`"}], ",", 
RowBox[{"-", "90.79`"}], ",", "5", ",", "1.5`", ",", "15.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45138\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45139"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "39.61`", ",", 
RowBox[{"-", "36.46`"}], ",", 
RowBox[{"-", "145.02`"}], ",", "5", ",", "2.5`", ",", "15.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45139\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45140"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "32.6`", ",", 
RowBox[{"-", "31.81`"}], ",", 
RowBox[{"-", "799.64`"}], ",", "8.47`", ",", "2.99`", ",", "30", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45140\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45173"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "25.05`", ",", 
RowBox[{"-", "17.27`"}], ",", 
RowBox[{"-", "49.33`"}], ",", "2.3`", ",", "1", ",", "6.25`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45173\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45174"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "17.97`", ",", 
RowBox[{"-", "11.2`"}], ",", 
RowBox[{"-", "85.31`"}], ",", "6.27`", ",", "1.76`", ",", "15.`", ",", "\"\<S-BAH11\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45174\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45175"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "21.41`", ",", 
RowBox[{"-", "13.75`"}], ",", 
RowBox[{"-", "105.58`"}], ",", "8.4`", ",", "1.76`", ",", "20", ",", "\"\<S-BAH11\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45175\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45178"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "194.96`", ",", 
RowBox[{"-", "148.27`"}], ",", 
RowBox[{"-", "483.71`"}], ",", "9", ",", "4.1`", ",", "40", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45178\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45179INK"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "130.48`", ",", 
RowBox[{"-", "99.36`"}], ",", 
RowBox[{"-", "320.2`"}], ",", "9", ",", "3.5`", ",", "50", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45179INK\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45179"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "130.48`", ",", 
RowBox[{"-", "99.36`"}], ",", 
RowBox[{"-", "320.2`"}], ",", "9", ",", "3.5`", ",", "50", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45179\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45180"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "162.59`", ",", 
RowBox[{"-", "123.82`"}], ",", 
RowBox[{"-", "402.58`"}], ",", "9.75`", ",", "3.5`", ",", "50", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45180\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45181"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "173.11`", ",", 
RowBox[{"-", "164.03`"}], ",", 
RowBox[{"-", "709.83`"}], ",", "9", ",", "3.5`", ",", "50", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45181\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45205"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "5.76`", ",", 
RowBox[{"-", "3.49`"}], ",", 
RowBox[{"-", "26.05`"}], ",", "1.5`", ",", "1.03`", ",", "4", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45205\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45206"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.17`", ",", 
RowBox[{"-", "4.39`"}], ",", 
RowBox[{"-", "33.96`"}], ",", "1.73`", ",", "1.03`", ",", "5.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45206\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45207"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "10.67`", ",", 
RowBox[{"-", "6.46`"}], ",", 
RowBox[{"-", "53.57`"}], ",", "1.55`", ",", "1.03`", ",", "5.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45207\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45208"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "6.98`", ",", 
RowBox[{"-", "4.35`"}], ",", 
RowBox[{"-", "41.01`"}], ",", "3.06`", ",", "1.03`", ",", "6.25`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45208\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45209"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "10.6`", ",", 
RowBox[{"-", "7.72`"}], ",", 
RowBox[{"-", "56.54`"}], ",", "6.23`", ",", "1.5`", ",", "12.5`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45209\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45210"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "22.94`", ",", 
RowBox[{"-", "15.54`"}], ",", 
RowBox[{"-", "58.84`"}], ",", "3.68`", ",", "1.5`", ",", "12.5`", ",", "\"\<N-SK11\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45210\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45211"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "21.17`", ",", 
RowBox[{"-", "16.08`"}], ",", 
RowBox[{"-", "118.66`"}], ",", "11.04`", ",", "3", ",", "25.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45211\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45212"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "28.75`", ",", 
RowBox[{"-", "21.38`"}], ",", 
RowBox[{"-", "82.92`"}], ",", "7.89`", ",", "3", ",", "25.`", ",", "\"\<N-SK11\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45212\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45213"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "55.68`", ",", 
RowBox[{"-", "38.37`"}], ",", 
RowBox[{"-", "146.45`"}], ",", "5.01`", ",", "3", ",", "25.`", ",", "\"\<N-SK11\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45213\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45214"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "139.53`", ",", 
RowBox[{"-", "100.21`"}], ",", 
RowBox[{"-", "291.32`"}], ",", "3", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45214\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45215"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "184.84`", ",", 
RowBox[{"-", "134.06`"}], ",", 
RowBox[{"-", "393.98`"}], ",", "3", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45215\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45216"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "244.65`", ",", 
RowBox[{"-", "179.62`"}], ",", 
RowBox[{"-", "534.1`"}], ",", "3", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45216\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45217"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "81.53`", ",", 
RowBox[{"-", "56.59`"}], ",", 
RowBox[{"-", "220.08`"}], ",", "4.92`", ",", "2.5`", ",", "30", ",", "\"\<N-SK11\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45217\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45218"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "37.33`", ",", 
RowBox[{"-", "31.54`"}], ",", 
RowBox[{"-", "124.51`"}], ",", "14.46`", ",", "3", ",", "40", ",", "\"\<N-SK11\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45218\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45262"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.39`", ",", 
RowBox[{"-", "5.15`"}], ",", 
RowBox[{"-", "14.62`"}], ",", "1.68`", ",", "1", ",", "3.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45262\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45263"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "30.77`", ",", 
RowBox[{"-", "22.05`"}], ",", 
RowBox[{"-", "64.6`"}], ",", "2", ",", "1", ",", "6.25`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45263\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45264"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "22.16`", ",", 
RowBox[{"-", "15.98`"}], ",", 
RowBox[{"-", "46.14`"}], ",", "2.5`", ",", "1.5`", ",", "9.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45264\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45265"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "61.54`", ",", 
RowBox[{"-", "44.39`"}], ",", 
RowBox[{"-", "130.26`"}], ",", "3", ",", "1.5`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45265\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45266"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "76.92`", ",", 
RowBox[{"-", "55.48`"}], ",", 
RowBox[{"-", "162.59`"}], ",", "4.5`", ",", "2", ",", "18", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45266\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45267"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "35.92`", ",", 
RowBox[{"-", "28.12`"}], ",", 
RowBox[{"-", "83.79`"}], ",", "5", ",", "2", ",", "20", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45267\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45268"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "290.71`", ",", 
RowBox[{"-", "232.07`"}], ",", 
RowBox[{"-", "750.52`"}], ",", "3", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45268\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45269"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "184.4`", ",", 
RowBox[{"-", "133.95`"}], ",", 
RowBox[{"-", "394.96`"}], ",", "4.5`", ",", "2.5`", ",", "30", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45269\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45270"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "305.31`", ",", 
RowBox[{"-", "224.08`"}], ",", 
RowBox[{"-", "667.68`"}], ",", "6", ",", "4", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45270\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45271"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "305.74`", ",", 
RowBox[{"-", "223.2`"}], ",", 
RowBox[{"-", "663.82`"}], ",", "8", ",", "4", ",", "50", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45271\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45345"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "37.16`", ",", 
RowBox[{"-", "26.28`"}], ",", 
RowBox[{"-", "76.48`"}], ",", "2.3`", ",", "1", ",", "6.25`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45345\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45346"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "8.37`", ",", 
RowBox[{"-", "7.01`"}], ",", 
RowBox[{"-", "26.18`"}], ",", "5", ",", "2", ",", "9.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF57\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45346\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45347"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "27.83`", ",", 
RowBox[{"-", "19.75`"}], ",", 
RowBox[{"-", "57.06`"}], ",", "3", ",", "1.3`", ",", "9.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45347\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45348"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "20.73`", ",", 
RowBox[{"-", "14.31`"}], ",", 
RowBox[{"-", "133.58`"}], ",", "6.91`", ",", "2", ",", "18", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45348\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45349"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "42.51`", ",", 
RowBox[{"-", "26.24`"}], ",", 
RowBox[{"-", "226.77`"}], ",", "3.85`", ",", "2", ",", "18", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45349\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45350"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "53.03`", ",", 
RowBox[{"-", "32.72`"}], ",", 
RowBox[{"-", "288.05`"}], ",", "3.27`", ",", "2", ",", "18", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45350\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45351"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "24.24`", ",", 
RowBox[{"-", "15.96`"}], ",", 
RowBox[{"-", "139.78`"}], ",", "7.33`", ",", "1.76`", ",", "20", ",", "\"\<S-BAH11\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45351\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45352"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "279.6`", ",", 
RowBox[{"-", "199.51`"}], ",", 
RowBox[{"-", "579.39`"}], ",", "6", ",", "4", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45352\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45353"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "69.28`", ",", 
RowBox[{"-", "46.95`"}], ",", 
RowBox[{"-", "446.01`"}], ",", "13.92`", ",", "4", ",", "50", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45353\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45354"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "227.16`", ",", 
RowBox[{"-", "174.83`"}], ",", 
RowBox[{"-", "571.49`"}], ",", "8", ",", "4", ",", "50", ",", "\"\<N-BAK4\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45354\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45407"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "5.56`", ",", 
RowBox[{"-", "3.09`"}], ",", 
RowBox[{"-", "18.25`"}], ",", "3.72`", ",", "1.03`", ",", "5.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45407\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45408"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "14.11`", ",", 
RowBox[{"-", "8.63`"}], ",", 
RowBox[{"-", "75.59`"}], ",", "1.61`", ",", "1.03`", ",", "5.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45408\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45409INK"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "49.62`", ",", 
RowBox[{"-", "35.73`"}], ",", 
RowBox[{"-", "103.22`"}], ",", "2.1`", ",", "1.6`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45409INK\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45409"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "49.62`", ",", 
RowBox[{"-", "35.73`"}], ",", 
RowBox[{"-", "103.22`"}], ",", "2.1`", ",", "1.6`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45409\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45410"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "55.8`", ",", 
RowBox[{"-", "40.17`"}], ",", 
RowBox[{"-", "116.32`"}], ",", "1.98`", ",", "1.6`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45410\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45412"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "18.7`", ",", 
RowBox[{"-", "13.11`"}], ",", 
RowBox[{"-", "119.12`"}], ",", "7.67`", ",", "1.6`", ",", "18", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45412\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45413"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "24.26`", ",", 
RowBox[{"-", "16.23`"}], ",", 
RowBox[{"-", "152.99`"}], ",", "6.01`", ",", "1.6`", ",", "18", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45413\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45414"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "171.03`", ",", 
RowBox[{"-", "121.54`"}], ",", 
RowBox[{"-", "352.46`"}], ",", "5", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45414\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45415"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "123.77`", ",", 
RowBox[{"-", "89.22`"}], ",", 
RowBox[{"-", "259.43`"}], ",", "5", ",", "2.5`", ",", "30", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45415\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45417"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "118.81`", ",", 
RowBox[{"-", "96.37`"}], ",", 
RowBox[{"-", "288.97`"}], ",", "17.94`", ",", "6", ",", "75.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45417\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45418"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "184.05`", ",", 
RowBox[{"-", "137.34`"}], ",", 
RowBox[{"-", "399.33`"}], ",", "13.59`", ",", "6", ",", "75.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45418\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45419"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "246.5`", ",", 
RowBox[{"-", "180.92`"}], ",", 
RowBox[{"-", "526.7`"}], ",", "11.05`", ",", "6", ",", "75.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45419\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45818"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "24.77`", ",", 
RowBox[{"-", "18.01`"}], ",", 
RowBox[{"-", "51.17`"}], ",", "3.73`", ",", "1.33`", ",", "9.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45818\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45819"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "36.51`", ",", 
RowBox[{"-", "26.33`"}], ",", 
RowBox[{"-", "78.35`"}], ",", "3.5`", ",", "1.5`", ",", "9.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45819\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45820"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "46.44`", ",", 
RowBox[{"-", "33.77`"}], ",", 
RowBox[{"-", "95.94`"}], ",", "3", ",", "1.6`", ",", "9.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45820\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47721"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "3.16`", ",", 
RowBox[{"-", "2.23`"}], ",", 
RowBox[{"-", "7.88`"}], ",", "2", ",", "1", ",", "3.`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47721\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49291"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "51.88`", ",", 
RowBox[{"-", "32.79`"}], ",", 
RowBox[{"-", "309.45`"}], ",", "20", ",", "4.5`", ",", "50", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49291\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49758"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "10.59`", ",", 
RowBox[{"-", "8.26`"}], ",", 
RowBox[{"-", "30.7`"}], ",", "6.5`", ",", "2", ",", "12.7`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF57\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49758\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49759"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "13.184931506849315`", ",", 
RowBox[{"-", "10.21505376344086`"}], ",", 
RowBox[{"-", "50.875`"}], ",", "5.25`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49759\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49760"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "16.77`", ",", 
RowBox[{"-", "13.63`"}], ",", 
RowBox[{"-", "50.73`"}], ",", "4", ",", "1.75`", ",", "12.7`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49760\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49761"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "21.13`", ",", 
RowBox[{"-", "16.69`"}], ",", 
RowBox[{"-", "61.73`"}], ",", "4", ",", "1.75`", ",", "12.7`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49761\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49762"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "25.425`", ",", 
RowBox[{"-", "20.03`"}], ",", 
RowBox[{"-", "74.39`"}], ",", "3.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49762\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49763"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "28.49`", ",", 
RowBox[{"-", "20.03`"}], ",", 
RowBox[{"-", "81.41`"}], ",", "3", ",", "2", ",", "12.7`", ",", "\"\<N-BAK1\>\"", ",", "\"\<N-SF8\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49763\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49764"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "33.16494845360825`", ",", 
RowBox[{"-", "23.975`"}], ",", 
RowBox[{"-", "89.025`"}], ",", "2.5`", ",", "1.5`", ",", "12.7`", ",", "\"\<N-BAK1\>\"", ",", "\"\<N-SF15\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49764\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49765"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "26.37`", ",", 
RowBox[{"-", "20.43`"}], ",", 
RowBox[{"-", "101.75`"}], ",", "10.5`", ",", "3", ",", "25.4`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49765\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"49766"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "33.54`", ",", 
RowBox[{"-", "27.27`"}], ",", 
RowBox[{"-", "101.5`"}], ",", "8", ",", "3.5`", ",", "25.4`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49766\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49767"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "42.26`", ",", 
RowBox[{"-", "33.37`"}], ",", 
RowBox[{"-", "123.45`"}], ",", "8", ",", "3.5`", ",", "25.4`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49767\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49768"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "50.85`", ",", 
RowBox[{"-", "40.06`"}], ",", 
RowBox[{"-", "148.78`"}], ",", "7", ",", "3", ",", "25.4`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49768\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49769"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "56.98`", ",", 
RowBox[{"-", "40.06`"}], ",", 
RowBox[{"-", "162.82`"}], ",", "6", ",", "4", ",", "25.4`", ",", "\"\<N-BAK1\>\"", ",", "\"\<N-SF8\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49769\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49770"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "66.33`", ",", 
RowBox[{"-", "47.95`"}], ",", 
RowBox[{"-", "178.05`"}], ",", "5", ",", "3", ",", "25.4`", ",", "\"\<N-BAK1\>\"", ",", "\"\<N-SF15\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49770\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49923"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "8.57`", ",", 
RowBox[{"-", "6.22`"}], ",", 
RowBox[{"-", "21.36`"}], ",", "2.3`", ",", "1", ",", "5.`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49923\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49924"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "5.6`", ",", 
RowBox[{"-", "3.71`"}], ",", 
RowBox[{"-", "11.34`"}], ",", "5", ",", "1", ",", "6.25`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF57\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49924\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49925"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "11.125`", ",", 
RowBox[{"-", "9.22`"}], ",", 
RowBox[{"-", "38.49`"}], ",", "2.75`", ",", "1", ",", "6.25`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49925\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49926"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "10.49`", ",", 
RowBox[{"-", "8.08`"}], ",", 
RowBox[{"-", "38.23`"}], ",", "4", ",", "1.5`", ",", "9.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49926\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49927"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "14.79`", ",", 
RowBox[{"-", "12.09`"}], ",", 
RowBox[{"-", "45.72`"}], ",", "3", ",", "1.5`", ",", "9.`", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49927\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49928"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "12.2`", ",", 
RowBox[{"-", "9.82`"}], ",", 
RowBox[{"-", "39.19`"}], ",", "5.8`", ",", "1.5`", ",", "12.5`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF57\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49928\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49929"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "14.01`", ",", 
RowBox[{"-", "11.36`"}], ",", 
RowBox[{"-", "44.21`"}], ",", "6.8`", ",", "2", ",", "15.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF57\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49929\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49930"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "37.23`", ",", 
RowBox[{"-", "26.69`"}], ",", 
RowBox[{"-", "76.36`"}], ",", "3.5`", ",", "2", ",", "15.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49930\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49931"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "15.83`", ",", 
RowBox[{"-", "12.65`"}], ",", 
RowBox[{"-", "47.42`"}], ",", "8.7`", ",", "3", ",", "18", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF57\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49931\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49932"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "18.03`", ",", 
RowBox[{"-", "13.27`"}], ",", 
RowBox[{"-", "55.46`"}], ",", "9.5`", ",", "2.5`", ",", "20", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49932\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49933INK"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "36.92`", ",", 
RowBox[{"-", "25.65`"}], ",", 
RowBox[{"-", "77.89`"}], ",", "4.8`", ",", "2", ",", "20", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49933INK\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49933"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "36.92`", ",", 
RowBox[{"-", "25.65`"}], ",", 
RowBox[{"-", "77.89`"}], ",", "4.8`", ",", "2", ",", "20", ",", "\"\<N-SSK8\>\"", ",", "\"\<N-SF56\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49933\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"49934"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "50.49`", ",", 
RowBox[{"-", "37.06`"}], ",", 
RowBox[{"-", "102.19`"}], ",", "4", ",", "2", ",", "20", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<49934\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63690"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "4.24`", ",", 
RowBox[{"-", "2.6`"}], ",", 
RowBox[{"-", "18.9`"}], ",", "2.8`", ",", "1", ",", "4", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63690\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63691"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.12`", ",", 
RowBox[{"-", "4.22`"}], ",", 
RowBox[{"-", "33.66`"}], ",", "2", ",", "1", ",", "4", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63691\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63692"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.35`", ",", 
RowBox[{"-", "5.09`"}], ",", 
RowBox[{"-", "14.6`"}], ",", "1.8`", ",", "1", ",", "4", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63692\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63693"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "10.04`", ",", 
RowBox[{"-", "7.`"}], ",", 
RowBox[{"-", "80.71`"}], ",", "5.5`", ",", "2", ",", "10", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63693\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63694"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "14.15`", ",", 
RowBox[{"-", "8.38`"}], ",", 
RowBox[{"-", "71.22`"}], ",", "4", ",", "1", ",", "10", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63694\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63695"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "15.37`", ",", 
RowBox[{"-", "11.1`"}], ",", 
RowBox[{"-", "31.47`"}], ",", "3.5`", ",", "1.5`", ",", "10", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63695\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63696"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "18.58`", ",", 
RowBox[{"-", "13.17`"}], ",", 
RowBox[{"-", "37.11`"}], ",", "3.5`", ",", "2", ",", "10", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63696\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63697"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "23.66`", ",", 
RowBox[{"-", "18.11`"}], ",", 
RowBox[{"-", "55.78`"}], ",", "3", ",", "1.5`", ",", "10", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63697\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63698"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "26.22`", ",", 
RowBox[{"-", "23.95`"}], ",", 
RowBox[{"-", "97.55`"}], ",", "3", ",", "1.5`", ",", "10", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63698\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63699"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "60.99`", ",", 
RowBox[{"-", "43.74`"}], ",", 
RowBox[{"-", "130.98`"}], ",", "2.5`", ",", "1.5`", ",", "10", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63699\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63700"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "9.55`", ",", 
RowBox[{"-", "8.6`"}], ",", 
RowBox[{"-", "48.5`"}], ",", "6", ",", "1", ",", "12.5`", ",", "\"\<N-LAK22\>\"", ",", "\"\<N-SF6\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63700\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"63701"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "46.73`", ",", 
RowBox[{"-", "50.61`"}], ",", 
RowBox[{"-", "341.7`"}], ",", "10", ",", "3", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63701\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"65549"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "6.87`", ",", 
RowBox[{"-", "5.`"}], ",", 
RowBox[{"-", "14.16`"}], ",", "6.35`", ",", "1.8`", ",", "9.`", ",", "\"\<N-BASF64\>\"", ",", "\"\<N-SF66\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65549\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"65550"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "9.53`", ",", 
RowBox[{"-", "6.92`"}], ",", 
RowBox[{"-", "20.07`"}], ",", "8.12`", ",", "2.5`", ",", "12.5`", ",", "\"\<N-BASF64\>\"", ",", "\"\<N-SF66\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65550\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"65551"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "11.393063583815028`", ",", 
RowBox[{"-", "8.523076923076923`"}], ",", 
RowBox[{"-", "24.69`"}], ",", "9.86`", ",", "3", ",", "15.`", ",", "\"\<N-BASF64\>\"", ",", "\"\<N-SF66\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65551\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"65552"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "15.1340206185567`", ",", 
RowBox[{"-", "11.18`"}], ",", 
RowBox[{"-", "33.15`"}], ",", "12.57`", ",", "4", ",", "20", ",", "\"\<N-BASF64\>\"", ",", "\"\<N-SF66\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65552\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"65553"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "18.85`", ",", 
RowBox[{"-", "13.84`"}], ",", 
RowBox[{"-", "41.7`"}], ",", "15.33`", ",", "5", ",", "25.`", ",", "\"\<N-BASF64\>\"", ",", "\"\<N-SF66\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65553\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"65564"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "0.8`", ",", 
RowBox[{"-", "0.8`"}], ",", 
RowBox[{"-", "4.5`"}], ",", "0.5`", ",", "0.5`", ",", "1.`", ",", "\"\<N-PSK53A\>\"", ",", "\"\<N-LASF9\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65564\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"65566"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "1.75`", ",", 
RowBox[{"-", "1.75`"}], ",", 
RowBox[{"-", "7.13`"}], ",", "0.5`", ",", "0.5`", ",", "1.`", ",", "\"\<N-PSK53A\>\"", ",", "\"\<N-LASF9\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65566\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"65567"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "1.68`", ",", 
RowBox[{"-", "1.68`"}], ",", 
RowBox[{"-", "7.39`"}], ",", "1", ",", "1", ",", "2", ",", "\"\<N-PSK53A\>\"", ",", "\"\<N-LASF9\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65567\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"65568"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "2.26`", ",", 
RowBox[{"-", "2.26`"}], ",", 
RowBox[{"-", "10.`"}], ",", "1", ",", "1", ",", "2", ",", "\"\<N-PSK53A\>\"", ",", "\"\<N-LASF9\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65568\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"67328"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "107.2`", ",", 
RowBox[{"-", "67.9`"}], ",", 
RowBox[{"-", "179.23`"}], ",", "3", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67328\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"67329"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "315.2`", ",", 
RowBox[{"-", "507.71`"}], ",", "\[Infinity]", ",", "2.5`", ",", "2.5`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67329\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"67330"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "315.18`", ",", 
RowBox[{"-", "506.69`"}], ",", "\[Infinity]", ",", "4", ",", "4", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<67330\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"83338"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "5.38`", ",", 
RowBox[{"-", "5.38`"}], ",", 
RowBox[{"-", "20.79`"}], ",", "1", ",", "1", ",", "2", ",", "\"\<N-PSK53A\>\"", ",", "\"\<N-LASF9\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<83338\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"83339"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "7.2`", ",", 
RowBox[{"-", "7.2`"}], ",", 
RowBox[{"-", "27.8`"}], ",", "1", ",", "1", ",", "2", ",", "\"\<N-PSK53A\>\"", ",", "\"\<N-LASF9\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<83339\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"83415"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "76.92`", ",", 
RowBox[{"-", "55.48`"}], ",", 
RowBox[{"-", "162.59`"}], ",", "4.5`", ",", "2", ",", "12.7`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<83415\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"89681"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "28.557971014492754`", ",", 
RowBox[{"-", "29.964912280701753`"}], ",", 
RowBox[{"-", "153.6659038901602`"}], ",", "19", ",", "7", ",", "40", ",", "\"\<N-SK11\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<89681\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"32496INK"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "34.81`", ",", 
RowBox[{"-", "22.12`"}], ",", 
RowBox[{"-", "203.48`"}], ",", "11", ",", "2.2`", ",", "30", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32496INK\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"32498INK"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "40.51`", ",", 
RowBox[{"-", "38.68`"}], ",", 
RowBox[{"-", "38.68`"}], ",", "8.4`", ",", "0.01`", ",", "30", ",", "\"\<N-SSK8\>\"", ",", "\"\<NOA61\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<32498INK\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<NOA61\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"33917"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "25.62`", ",", 
RowBox[{"-", "25.62`"}], ",", 
RowBox[{"-", "25.62`"}], ",", "10.9`", ",", "0.01`", ",", "30", ",", "\"\<N-BAF10\>\"", ",", "\"\<NOA61\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<33917\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<NOA61\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"33918"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "64.67`", ",", 
RowBox[{"-", "64.67`"}], ",", 
RowBox[{"-", "64.67`"}], ",", "26", ",", "0.01`", ",", "75.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<NOA61\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<33918\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<NOA61\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"33919INK"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "92.05`", ",", 
RowBox[{"-", "72.85`"}], ",", 
RowBox[{"-", "72.85`"}], ",", "23.2`", ",", "0.01`", ",", "75.`", ",", "\"\<N-BAK1\>\"", ",", "\"\<NOA61\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<33919INK\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<NOA61\>\""}], "}"}]}]}], "]"}],
HoldForm]\),"33919"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "92.05`", ",", 
RowBox[{"-", "72.85`"}], ",", 
RowBox[{"-", "72.85`"}], ",", "23.2`", ",", "0.01`", ",", "75.`", ",", "\"\<N-BAK1\>\"", ",", "\"\<NOA61\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<33919\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<NOA61\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"65549INK"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", "6.87`", ",", 
RowBox[{"-", "5.`"}], ",", 
RowBox[{"-", "5.`"}], ",", "6.35`", ",", "0.01`", ",", "9.`", ",", "\"\<N-BASF64\>\"", ",", "\"\<NOA61\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<65549INK\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\"", ",", "\"\<NOA61\>\""}], "}"}]}]}], "]"}],
HoldForm]\)|>;


(*ssociation[Map[readEdmundOpticsZemaxCementedDoubletSphericalLensFile[#,CharacterEncoding->"Unicode"]&,FileNames["*.zmx","D:\\FotometrMCB\\StockLenses\\EdmundOptics\\VisibleAchromaticNegativeCementedDoubletsMgF2Coated"]]]*)
edmundOpticsNegativeCementedDoubletsAchromaticVisible=<|"45219"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "17.75`"}], ",", "12.17`", ",", "109.75`", ",", "2", ",", "2.55`", ",", "12.5`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45219\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45220"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "30.83`"}], ",", "23.47`", ",", "69.2`", ",", "2", ",", "1.56`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45220\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45221"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "35.51`"}], ",", "24.58`", ",", "217.99`", ",", "2.5`", ",", "4.06`", ",", "25.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45221\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45222"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "70.57`"}], ",", "45.27`", ",", "412.67`", ",", "2", ",", "2.57`", ",", "25.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45222\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45420"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "24.66`"}], ",", "4.24`", ",", "11.25`", ",", "1", ",", "2", ",", "6.25`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45420\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45421"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "17.57`"}], ",", "11.48`", ",", "109.44`", ",", "1", ",", "2", ",", "6.25`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45421\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45422"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "46.07`"}], ",", "36.65`", ",", "108.`", ",", "2.5`", ",", "4.06`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45422\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45423"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "92.3`"}], ",", "68.87`", ",", "204.15`", ",", "2.5`", ",", "4.06`", ",", "25.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45423\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45424"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "21.66`"}], ",", "17.48`", ",", "50.54`", ",", "2", ",", "1.56`", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45424\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"45828"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "17.75`"}], ",", "12.17`", ",", "109.75`", ",", "2", ",", "2.55`", ",", "9.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<45828\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"47016"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "46.07`"}], ",", "36.65`", ",", "108.`", ",", "2.5`", ",", "3", ",", "12.5`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<47016\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"48349"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "35.51`"}], ",", "6.11`", ",", "16.2`", ",", "1.44`", ",", "2.88`", ",", "9.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<48349\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"48350"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "10.62`"}], ",", "8.63`", ",", "81.12`", ",", "2", ",", "4", ",", "12.5`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<48350\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"48351"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "21.24`"}], ",", "17.26`", ",", "162.24`", ",", "4", ",", "8", ",", "25.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<48351\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"62472"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "4.55`"}], ",", "4.55`", ",", "\[Infinity]", ",", "1", ",", "3", ",", "6.25`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<62472\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"62473"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "6.544910179640719`"}], ",", "5.1`", ",", "89.1`", ",", "1", ",", "2.5`", ",", "6.25`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<62473\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"62474"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "6.98`"}], ",", "6.98`", ",", "23.22`", ",", "1.5`", ",", "2.5`", ",", "9.`", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<62474\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"62475INK"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "13.8`"}], ",", "9.99`", ",", "109.48`", ",", "2", ",", "3.5`", ",", "12.5`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<62475INK\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"62475"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "13.8`"}], ",", "9.99`", ",", "109.48`", ",", "2", ",", "3.5`", ",", "12.5`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<62475\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"62476"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "27.82`"}], ",", "19.65`", ",", "201.68`", ",", "3", ",", "5.5`", ",", "25.`", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<62476\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"63763"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "41.26`"}], ",", "30.56`", ",", "335.88`", ",", "5", ",", "8.5`", ",", "40", ",", "\"\<N-BAF10\>\"", ",", "\"\<N-SF10\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63763\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"63764"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "47.32`"}], ",", "40.51`", ",", "128.26`", ",", "4", ",", "6.5`", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63764\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"63765"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "60.11`"}], ",", "48.43`", ",", "149.5`", ",", "4", ",", "6.5`", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63765\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"63766"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "72.65`"}], ",", "56.77`", ",", "173.4`", ",", "4", ",", "6.5`", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63766\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\),
"63767"->\!\(\*
TagBox[
RowBox[{"OpenRayTracer`createCementedSphericalDoubletLens", "[", 
RowBox[{"0", ",", 
RowBox[{"-", "91.23`"}], ",", "69.66`", ",", "211.28`", ",", "4", ",", "6.5`", ",", "40", ",", "\"\<N-BK7\>\"", ",", "\"\<N-SF5\>\"", ",", 
RowBox[{"OpenRayTracer`PartId", "->", "\"\<63767\>\""}], ",", 
RowBox[{"OpenRayTracer`GlassCatalogueData", "->", 
RowBox[{"{", 
RowBox[{"\"\<SCHOTT\>\"", ",", "\"\<OHARA\>\"", ",", "\"\<MISC\>\"", ",", "\"\<CORNING\>\"", ",", "\"\<INFRARED\>\""}], "}"}]}]}], "]"}],
HoldForm]\)|>;


edmundOpticsSingletLenses={edmundOpticsFusedOpticsBiConcave,edmundOpticsBiConcave,edmundOpticsCaF2BiConvex,edmundOpticsFusedSilicaBiConvex,edmundOpticsBiConvex,edmundOpticsCaF2PlanoConvex,edmundOpticsFusedSilicaLargePlanoConvex,
edmundOpticsFusedSilicaLambdaTwenthiethPlanoConvex,edmundOpticsFusedSilicaUltraThinLaserGradePlanoConvex,edmundOpticsFusedSilicaLaserGradePlanoConvex,edmundOpticsFusedSilicaPlanoConcave,edmundOpticsFusedSilicaPlanoConvex,edmundOpticsPlanoConvex,edmundOpticsPlanoConcave,
edmundOpticsPrecisionAspheres,edmundOpticsFusedSilicaPrecisionAspheres,edmundOpticsLambda40Aspheres,edmundOpticsBestFormAspheres};


edmundOpticsCementedDoubletLenses={edmundOpticsPositiveCementedDoubletsAchromaticVisible,edmundOpticsNegativeCementedDoubletsAchromaticVisible};


edmundOpticsAllLenses=Join[edmundOpticsSingletLenses,edmundOpticsCementedDoubletLenses];


allLenses= <|thorlabsManufacturerName->thorlabsAllLenses, edmundOpticsManufacturerName->edmundOpticsAllLenses|>;


End[];


(* ::Input::Initialization:: *)
EndPackage[]
