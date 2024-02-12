(* ::Package:: *)

BeginPackage["Swarmica`"];


ExtractK::usage= "ExtractK[za_, path_, kRange_:{0,1,0.1}], returns accuracy, regret, inaccuracy, indecision, conv times and stds "


PlotK::usage = "PlotK[za, path,G:4, Ns:{100,1000,10000}, kRange:{0,1,0.1}] plots accuracy/k, regrets/k, convergence time/k, inaccuracy/k, indecision/k and cognitive cost/accuracy graphs for a specific value of za of a heterogeneous system . Ns is the list of system sizes for which to plot cognitive cost/accuracy . The files it takes must be inside the path directory . "


PlotG::usage = "PlotG[za, path, Ns:{100,1000,10000}, gRange:{2,25,1}] plots accuracy/G, regrets/G, convergence time/G and cognitive cost/accuracy graphs for a specific value of za of a homogeneous combinatorial sytem . Ns is the list of system sizes for which to plot cognitive cost/accuracy . The files it takes must be inside the path directory . "


PlotKZSum::usage = "PlotKZSum[zSum_, relativePath, krange_:{0,1,0.1}] plots accuracy/k, regret/k, convergence time/k and convergence time/accuracy for a specific value of za+zb of a heterogeneous system stored in the relativePath directory . "


WriteHeteroData::usage = "WriteHeteroData[system, za, path, kRange={0,1,0.1}, eps=0, tol=0.7, zbRange={0,0.5,0.005}, qRange={0,1,0.01}, tmax=500000000] creates the files with the convergence time, the points (zb,q) for which A wins, B wins and the deadlock points for values of k in kRange in a heterogeneous system . It does so by integrating the ODEs specified as system for t->tmax and checking the last value of the total A population: if it is >= than tol, it is added to the A points, if it is <= tol, it is added to the B points and it is added to the deadlock points otherwise . "


WriteHomoData::usage="WriteHomoData[system, za, path, gRange={2,25,1}, eps=0, tol=0.7, zbRange={0,0.5,0.005}, qRange={0,1,0.01}, tmax=500000000] creates the files with the convergence time, the points (zb,q) for which A wins, B wins and the deadlock points for the values of G in gRange in a homogeneous system . It does so by integrating the ODEs specified as system for t->tmax and checking the last value of the total A population: if it is >= than tol, it is added to the A points, if it is <= tol, it is added to the B points and it is added to the deadlock points otherwise . "


ParameterSpaceK::usage="ParameterSpaceK[k, za, path] plots the parameter space associated to za and k . The files containing the points must be in the path directory . "


CognitiveCost::usage="CognitiveCost[za, path, Gs:4, N:100, kRange:{0,1,0.1}] plots the accuracy/cognitive cost for a heterogeneous model whose results are found in the path directory, with G neighbours and Ns being the list of the number of agents . "


MinDistance::usage="MinDistance[costAccuracyList, point:{0,0.5}] finds the point in costAccuracyList at the minimum euclidian distance from point . Prints that point . "


CompareInitialConditionsK::usage="CompareInitialConditionsK[za_, path_]"


GetAveragedMetrics::usage="GetAveragedMetrics[za, path1, path2, path3, n, G] returns the averaged metrics in a list built as {accuracies, regrets, convergenceTimes}"


PlotRegret::usage="PlotRegret[path_, k_,za_] returns a plot of the regret as a ListDensityPlot"


Begin["`Private`"];


PlotRegret[path_, k_,za_]:=Module[{regretValueList = {}},
awinspoints = ToExpression[StringSplit[Import[path<>"a_wins_points_k_"<>ToString[NumberDigit[k, 0]]<>"_"<>ToString[NumberDigit[k, -1]]<>"_"<>ToString[NumberDigit[k, -2]]<>"_za_"<>ToString[NumberDigit[za,0]]<>"_"<>ToString[NumberDigit[za,-1]]<>"_"<>ToString[NumberDigit[za,-2]]<>".txt"],"\n"]];
bwinspoints =ToExpression[StringSplit[ Import[path<>"b_wins_points_k_"<>ToString[NumberDigit[k, 0]]<>"_"<>ToString[NumberDigit[k, -1]]<>"_"<>ToString[NumberDigit[k, -2]]<>"_za_"<>ToString[NumberDigit[za,0]]<>"_"<>ToString[NumberDigit[za,-1]]<>"_"<>ToString[NumberDigit[za,-2]]<>".txt"],"\n"]];
deadlockpoints =ToExpression[StringSplit[Import[path<>"deadlock_points_k_"<>ToString[NumberDigit[k, 0]]<>"_"<>ToString[NumberDigit[k, -1]]<>"_"<>ToString[NumberDigit[k, -2]]<>"_za_"<>ToString[NumberDigit[za,0]]<>"_"<>ToString[NumberDigit[za,-1]]<>"_"<>ToString[NumberDigit[za,-2]]<>".txt"],"\n"]];

Do[
AppendTo[regretValueList,{awinspoints[[i]][[1]], awinspoints[[i]][[2]], 0}];
,{i, 1, Length[awinspoints]}];
Do[
AppendTo[regretValueList,{bwinspoints[[i]][[1]], bwinspoints[[i]][[2]], 1- bwinspoints[[i]][[2]]}];
,{i, 1, Length[bwinspoints]}];
Do[
AppendTo[regretValueList,{deadlockpoints[[i]][[1]], deadlockpoints[[i]][[2]], 1}];
,{i, 1, Length[deadlockpoints]}];

plt2 = ListDensityPlot[regretValueList, ColorFunction->"BlueGreenYellow",Frame->True, PlotLabel->Style["k="<>ToString[k], Bold, Black, 24],
							FrameTicks->{True,True},
							 ImageSize->Medium, PlotRange->All,PlotRangeClipping->False,
ColorFunctionScaling->True,ClippingStyle->Automatic, PlotRangePadding -> {0.005, 0.01}, FrameLabel->{Style["\!\(\*SubscriptBox[\(z\), \(b\)]\)",Bold, 24, Black, Italic],Style["q",Bold, 24, Black, Italic]
							(*Row[{
								 Style["quality ratio (", Bold, 72, Black],
								  Style["q", Bold, 72, FontSlant -> Italic, Black],
								    Style[")", Bold, 72, Black]
								  }]*)}];
plt2
]


CognitiveCost[za_, path_, Gs_:{4,6,8}, N_:100, kRange_:{0,1,0.1}]:=Module[{measures = ExtractK[za, path, {0,1,0.1}]}, 
accuracies = measures[[1]];
regrets = measures[[2]];
convergenceTimes = measures[[3]];
Do[costAccuracyList=List[];
	Do[
	cost=N*convergenceTimes[[Floor[(kRange[[2]]-kRange[[1]])/kRange[[3]]*k]+1]][[2]]*(k+(1-k)*(Gs[[i]]-1)); 
AppendTo[costAccuracyList, Labeled[{cost, accuracies[[Floor[(kRange[[2]]-kRange[[1]])/kRange[[3]]*k]+1]][[2]]}, Text["k="<>ToString[k]]]], {k, kRange[[1]], kRange[[2]], kRange[[3]]}];
Graphics[ListPlot[costAccuracyList, AxesLabel->{"Cognitive Cost ","Accuracy "}, PlotLabel->"za, N="<>ToString[za]<>", "<>ToString[Gs[[i]]], ImageSize->Full,PlotRange->Full]//Print];
minDist = +Infinity;
minDistPoint = {0,0};
Do[dist = EuclideanDistance[{0,0.5}, costAccuracyList[[i,1]]];
If[dist<minDist, minDist=dist; minDistPoint = costAccuracyList[[i]]]
,{i, 1,Length[costAccuracyList]}]
Print["minimum distance from {0, 0.5}: "<>ToString[minDist]];
Print["the closest point is " <>ToString[minDistPoint]];
,{i, 1, Length[Gs]}];

]


ParameterSpaceK[k_, za_, path_]:=Module[{accuracies={},regrets={}, convergenceTimes={}},
suffix = ToString[NumberDigit[k, 0]]<>"_"<>ToString[NumberDigit[k, -1]]<>"_"<>ToString[NumberDigit[k, -2]]<>"_za_"<>ToString[NumberDigit[za,0]]<>"_"<>ToString[NumberDigit[za,-1]]<>"_"<>ToString[NumberDigit[za,-2]]<>".txt";
awinspoints = ToExpression[StringSplit[Import[path<>"a_wins_points_k_"<>suffix],"\n"]] ;
bwinspoints =ToExpression[StringSplit[ Import[path<>"b_wins_points_k_"<>suffix],"\n"]];
deadlockpoints =ToExpression[StringSplit[Import[path<>"deadlock_points_k_"<>suffix],"\n"]];
If[Length[awinspoints]>0, If[Length[bwinspoints]>0, If[Length[deadlockpoints]>0,
						legend=SwatchLegend[Thread[Directive[{Red, Blue, Yellow},AbsoluteThickness[40]]],  
						{Pane["correct\ndecision", Alignment -> Center], Pane["incorrect\ndecision", Alignment -> Center], Pane["decision\ndeadlock", Alignment -> Center]},
					 LegendMarkers -> {Graphics[{Blue, Disk[]}, ImageSize -> 3000], Graphics[{Red, Disk[]}, ImageSize -> 30], Graphics[{Yellow, Disk[]}, ImageSize -> 30]},
					   LegendMarkerSize -> {{40, 40}},
						LegendFunction->"Frame",
						LabelStyle->{FontSize->48, Bold}, 
						LegendLayout->{"Row",1}];
						plot = Show[ListPlot[{awinspoints, bwinspoints, deadlockpoints}, 
							PlotStyle->{Blue, Red, Yellow},
							FrameLabel->{Style["proportion of zealots for B (\!\(\*SubscriptBox[\(z\), \(b\)]\))",Black, Bold, 72, SingleLetterItalics->True],
							Row[{
								 Style["quality ratio (", Bold, 72, Black],
								  Style["q", Bold, 72, FontSlant -> Italic, Black],
								    Style[")", Bold, 72, Black]
								  }]}, 
							ImageSize->Full, LabelStyle->Directive[Black, Large], 
							PlotMarkers->{"\[FilledCircle]", 30},
							AspectRatio->1,
							Frame->True,
							FrameTicks->True,
							FrameTicksStyle->Directive[Black,72],
							PlotRangePadding -> {Automatic, 0.02}
							]];
							Legended[plot, Placed[legend, Above]];
							plot
							,
						plot = ListPlot[{awinspoints, bwinspoints}, PlotStyle->{Blue, Red},
							FrameLabel->{Style["proportion of zealots B (\!\(\*SubscriptBox[\(z\), \(b\)]\))", Bold, 72, SingleLetterItalics->True],Style["quality ratio (q)", Bold, 72, SingleLetterItalics->True]}, 
							ImageSize->Full, LabelStyle->Directive[Black, Large], 
							PlotMarkers->{"\[FilledCircle]", 20},
							AspectRatio->1,
							Frame->True,
							FrameTicks->True,
							FrameTicksStyle->Directive[Black,72],
							PlotLegends->{"A wins ", "B wins "}]], Print["All A wins points "]], Print["No A wins points . "]]
]


WriteHomoData[system_, za_, path_, gRange_:{2,25,1}, eps_:0, tol_:0.7, zbRange_:{0,0.5,0.005}, qRange_:{0,1,0.01}, tmax_:500000000]:=Module[{}
,
Do[
Clear[Avm, Amr, Bvm, Bmr, tau];
awinspoints = {};
bwinspoints = {};
deadlockpoints = {};
convergenceTimes={};
convergenceTimeSum=0.0;
p[x_] :=1/(1+Exp[50-100*x]);
Do[
out = NDSolve[system, 
	{A,B},
	{t,0,tmax}];
plotA = Plot[Evaluate[{A[t]}/.out], {t,0,tmax}];
pointsA = plotA  //Cases[#,Line[x_]:>x,All]&//First;
plotB = Plot[Evaluate[{B[t]}/.out], {t,0,tmax}];
pointsB = plotB  //Cases[#,Line[x_]:>x,All]&//First;
If[
Last[pointsA][[2]]>=tol, 
AppendTo[awinspoints, {zb,q}];
For[s=0, s<tmax, s++, o = {A[s]}/.out; If[o[[1,1]]>=tol,convergenceTimeSum+=s;AppendTo[convergenceTimes, {zb,q,s}]; Break[]]], 
If[
Last[pointsB][[2]]>=tol, 
AppendTo[bwinspoints, {zb,q}], 
AppendTo[deadlockpoints, {zb,q}]
]];
,
{zb,zbRange[[1]], zbRange[[2]], zbRange[[3]]},{q,qRange[[1]],qRange[[2]], qRange[[3]]}];
Export[path<>"convergence_time _G _ "<>ToString[G]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", convergenceTimeSum/Length[awinspoints]];
Export[path<>"convergence_times _G _ "<>ToString[G]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", convergenceTimes];
Export[path<>"a_wins _points _G _ "<>ToString[G]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", awinspoints];
Export[path<>"b_wins _points _G _ "<>ToString[G]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", bwinspoints];
Export[path<>"deadlock_points _G _ "<>ToString[G]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", deadlockpoints];
Print[G];
,{G,gRange[[1]],gRange[[2]], gRange[[3]]}]] 


WriteHeteroData[system_, za_, path_, kRange_:{0,1,0.1}, eps_:0, tol_:0.7, zbRange_:{0,0.5,0.005}, qRange_:{0,1,0.01}, tmax_:500000000] := Module[{}
,
Do[
Clear[Avm, Amr, Bvm, Bmr, tau];
awinspoints = {};
bwinspoints = {};
deadlockpoints = {};
convergenceTimes={};
convergenceTimeSum=0.0;
p[x_] :=1/(1+Exp[50-100*x]);
Do[
out = NDSolve[system, 
	{Avm, Amr, Bvm, Bmr},
	{t,0,tmax}];
plotA = Plot[Evaluate[{Avm[t]+Amr[t]}/.out], {t,0,tmax}];
pointsA = plotA  //Cases[#,Line[x_]:>x,All]&//First;
plotB = Plot[Evaluate[{Bvm[t]+Bmr[t]}/.out], {t,0,tmax}];
pointsB = plotB  //Cases[#,Line[x_]:>x,All]&//First;
If[
Last[pointsA][[2]]>=tol, 
AppendTo[awinspoints, {zb,q}];
For[s=0, s<tmax, s++, o = {Avm[s]+Amr[s]}/.out; If[o[[1,1]]>=tol,convergenceTimeSum+=s;AppendTo[convergenceTimes, {zb,q,s}]; Break[]]], 
If[
Last[pointsB][[2]]>=tol, 
AppendTo[bwinspoints, {zb,q}], 
AppendTo[deadlockpoints, {zb,q}]
]];
,
{zb,zbRange[[1]], zbRange[[2]], zbRange[[3]]},{q,qRange[[1]],qRange[[2]], qRange[[3]]}]
Export[path<>"convergence_time _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", convergenceTimeSum/Length[awinspoints]];
Export[path<>"convergence_times _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", convergenceTimes];
Export[path<>"a_wins _points _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", awinspoints];
Export[path<>"b_wins _points _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", bwinspoints];
Export[path<>"deadlock_points _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt", deadlockpoints];
Print[k];
,{k,kRange[[1]],kRange[[2]], kRange[[3]]}] ]


PlotG[za_, path_,Ns_:{100,1000,10000}, gRange_:{2,25,1}]:=PreparePlotsG[za, ExtractG[za, path, gRange], Ns]


PreparePlotsG[za_, measures_, Ns_] :=Module[{accuracies=measures[[1]],regrets = measures[[2]],convergenceTimes = measures[[3]], plots={} },
Graphics[ListPlot[accuracies, AxesLabel->{"G ","Accuracy "}, PlotLabel->"za="<>ToString[za], ImageSize->Medium, PlotRange->Full]//Print];
Graphics[ListPlot[regrets, AxesLabel->{"G ","Regret "}, PlotLabel->"za="<>ToString[za], ImageSize->Medium, PlotRange->Full]//Print];
Graphics[ListPlot[convergenceTimes, AxesLabel->{"G ","Convergence Time "}, PlotLabel->"za="<>ToString[za], ImageSize->Medium, PlotRange->Full]//Print];
speedAccuracyList = {};
Do[
AppendTo[speedAccuracyList,Labeled[ {convergenceTimes[[i]][[2]], accuracies[[i]][[2]]}, Text["G="<>ToString[i]]]]
,{i,1,Length[accuracies]}];
Graphics[ListPlot[speedAccuracyList, AxesLabel->{"Convergence Time ","Accuracy "}, PlotLabel->"za="<>ToString[za], ImageSize->Full]];
Do[costAccuracyList=List[];
	Do[
	cost=Ns[[i]]*convergenceTimes[[j]][[2]]*j; (*IMPORTANT: assuming the first point is always G=2, then j=G-1*)
AppendTo[costAccuracyList, Labeled[{cost, accuracies[[j]][[2]]}, Text["G="<>ToString[j]]]], {j, 1, Length[accuracies]}];
Graphics[ListPlot[costAccuracyList, AxesLabel->{"Cognitive Cost ","Accuracy "}, PlotLabel->"za, N="<>ToString[za]<>", "<>ToString[Ns[[i]]], ImageSize->Full,PlotRange->Full]//Print];
,{i, 1, Length[Ns]}];
costAccuracyList
]


ExtractG[za_, path_, gRange_:{2,25,1}]:=Module[{accuracies={},regrets={}, convergenceTimes={} },Do[
awinspoints = ToExpression[StringSplit[Import[path<>"a_wins _points _G _ "<>ToString[G]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"],"\n "]] ;
bwinspoints =ToExpression[StringSplit[ Import[path<>"b_wins _points _G _ "<>ToString[G]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"],"\n "]];
deadlockpoints =ToExpression[StringSplit[Import[path<>"deadlock_points _G _ "<>ToString[G]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"],"\n "]];
convergenceTime = ToExpression[Import[path<>"convergence_time _G _ "<>ToString[G]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"]];
accuracy = Length[awinspoints]/(Length[awinspoints]+Length[bwinspoints]+Length[deadlockpoints]);
regret =0;
Do[
regret+=1-bwinspoints[[i]][[2]];
,{i, 1, Length[bwinspoints]}];
Do[
regret+=1;
,{i, 1, Length[deadlockpoints]}];
AppendTo[accuracies, {G, accuracy}];
AppendTo[regrets, {G,regret}];
AppendTo[convergenceTimes, {G,convergenceTime}];
, {G,gRange[[1]], gRange[[2]], gRange[[3]]}];
{accuracies, regrets, convergenceTimes}
 ]


PlotKZSum[zSum_, relativePath_, kRange_:{0,1,0.1}]:=PreparePlotsKZSum[zSum, ExtractKZSum[zSum, relativePath, kRange]]


PreparePlotsKZSum[zSum_, measures_]:=Module[{accuracies=measures[[1]],regrets = measures[[2]],convergenceTimes = measures[[3]], plots={} },
Graphics[ListPlot[accuracies, AxesLabel->{"k ","Accuracy "}, PlotLabel->"zSum="<>ToString[zSum]], ImageSize->Medium, PlotRange->Full//Print];
Graphics[ListPlot[regrets, AxesLabel->{"k ","Regret "}, PlotLabel->"zSum="<>ToString[zSum], PlotRange->Full]//Print];
Graphics[ListPlot[convergenceTimes, AxesLabel->{"k ","Convergence Time "}, PlotLabel->"zSum="<>ToString[zSum]], ImageSize->Medium, PlotRange->Full//Print];
speedAccuracyList = {};
Do[
AppendTo[speedAccuracyList,Labeled[ {convergenceTimes[[i]][[2]], accuracies[[i]][[2]]}, Text["k="<>ToString[i*0.1-0.1]]]]
,{i,1,Length[accuracies]}];
Graphics[ListPlot[speedAccuracyList, AxesLabel->{"Convergence Time ","Accuracy "}, PlotLabel->"zSum="<>ToString[zSum], ImageSize->Full], PlotRange->Full//Print];
]


ExtractKZSum[za_, subfolder_, kRange_]:=Module[{relativePath = "polimi/Magistrale/secondo_anno/thesis/notebooks/", accuracies={},regrets={}, convergenceTimes={} },Do[
awinspoints = ToExpression[StringSplit[Import[relativePath<>subfolder<>"a_wins _points _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_zSum _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"],"\n "]] ;
bwinspoints =ToExpression[StringSplit[ Import[relativePath<>subfolder<>"b_wins _points _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_zSum _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"],"\n "]];
deadlockpoints =ToExpression[StringSplit[Import[relativePath<>subfolder<>"deadlock_points _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_zSum _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"],"\n "]];
convergenceTime = ToExpression[Import[relativePath<>subfolder<>"convergence_time _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_zSum _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"]];
accuracy = Length[awinspoints]/(Length[awinspoints]+Length[bwinspoints]+Length[deadlockpoints]);
regret =0;
Do[
regret+=1-bwinspoints[[i]][[2]];
,{i, 1, Length[bwinspoints]}];
Do[
regret+=1;
,{i, 1, Length[deadlockpoints]}];
AppendTo[accuracies, {k, accuracy}];
AppendTo[regrets, {k,regret}];
AppendTo[convergenceTimes, {k,convergenceTime}];
, {k,kRange[[1]],kRange[[2]],kRange[[3]]}];
{accuracies, regrets, convergenceTimes}
 ]


PlotK[za_, path_, G_, Ns_:{100,1000,10000}, kRange_:{0,1,0.1}]:= PreparePlotsK[za, ExtractK[za, path,kRange],G, Ns, kRange]


CompareInitialConditionsK[za_, path_]:=Module[{}, 
moreaPath = path<>"more_a/za_"<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>"/";
morebPath = path<>"more_b/za_"<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>"/";
equalPath = path<>"equal/za_"<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>"/";
moreaMeasures = ExtractK[za, moreaPath];
morebMeasures = ExtractK[za, morebPath];
equalMeasures = ExtractK[za, equalPath];
moreaAccuracies = moreaMeasures[[1]];
morebAccuracies = morebMeasures[[1]];
equalAccuracies = equalMeasures[[1]];

moreaRegrets = moreaMeasures[[2]];
morebRegrets = morebMeasures[[2]];
equalRegrets = equalMeasures[[2]];

moreaConvergenceTimes = moreaMeasures[[3]];
morebConvergenceTimes = morebMeasures[[3]];
equalConvergenceTimes = equalMeasures[[3]];

moreaStds = moreaMeasures[[4]];
morebStds = morebMeasures[[4]];
equalStds = equalMeasures[[4]];

 GraphicsRow[ListPlot[moreaAccuracies, AxesLabel->{"k ","Accuracy "}, PlotLabel->"A(0)>B(0); za="<>ToString[za], PlotStyle->Blue]
 ,ListPlot[morebAccuracies, AxesLabel->{"k ","Accuracy "}, PlotLabel->"A(0)<B(0); za="<>ToString[za], PlotStyle->Red],
 ListPlot[equalAccuracies, AxesLabel->{"k ","Accuracy "}, PlotLabel->"A(0)=B(0); za="<>ToString[za], PlotStyle->Black]//Print]
]



PreparePlotsK[za_, measures_,G_, Ns_, kRange_]:=Module[{accuracies=measures[[1]],regrets = measures[[2]],convergenceTimes = measures[[3]], stds=measures[[4]],inaccuracies=measures[6], indecisions=measures[7], plots={} },
Graphics[ListPlot[accuracies, AxesLabel->{"k ","Accuracy "}, PlotLabel->"za="<>ToString[za]]//Print];
Graphics[ListPlot[regrets, AxesLabel->{"k ","Regret "}, PlotLabel->"za="<>ToString[za]]//Print];
Graphics[ListPlot[inaccuracies, AxesLabel->{"k ","Inaccuracy "}, PlotLabel->"za="<>ToString[za]]//Print];
Graphics[ListPlot[indecisions, AxesLabel->{"k ","Indecision "}, PlotLabel->"za="<>ToString[za]]//Print];
Graphics[ListPlot[stds, AxesLabel->{"k ","Convergence Time "}, PlotLabel->"za="<>ToString[za], PlotRange->Full, ImageSize->Large]//Print];
speedAccuracyList = {};
Do[
AppendTo[speedAccuracyList,Labeled[ {convergenceTimes[[i]][[2]], accuracies[[i]][[2]]}, Text["k="<>ToString[i*0.1-0.1]]]]
,{i,1,Length[accuracies]}];
Graphics[ListPlot[speedAccuracyList, AxesLabel->{"Convergence Time ","Accuracy "}, PlotLabel->"za="<>ToString[za], ImageSize->Full]];
Do[costAccuracyList=List[];
	Do[
	cost=Ns[[i]]*(convergenceTimes[[Floor[(kRange[[2]]-kRange[[1]])/kRange[[3]]*k]+1]][[2]])*(k+(1-k)*(G-1)); 
AppendTo[costAccuracyList, Labeled[{cost, accuracies[[Floor[(kRange[[2]]-kRange[[1]])/kRange[[3]]*k]+1]][[2]]}, Text["k="<>ToString[k]]]], {k, kRange[[1]], kRange[[2]], kRange[[3]]}];
Graphics[ListPlot[costAccuracyList, AxesLabel->{"Cognitive Cost ","Accuracy "}, PlotLabel->"za, N="<>ToString[za]<>", "<>ToString[Ns[[i]]], ImageSize->Full,PlotRange->Full]//Print],{i, 1, Length[Ns]}];
{accuracies, regrets, convergenceTimes, stds, inaccuracies, indecisions}
]


updatedTimeList = {};


ExtractK[za_, path_, kRange_:{0,1,0.05}]:=Module[{accuracies={},regrets={}, convergenceTimes={}, stds={}, convergenceTimesLists={}, inaccuracies={}, indecisions={} },Do[
suffix = ToString[NumberDigit[k, 0]]<>"_"<>ToString[NumberDigit[k, -1]]<>"_"<>ToString[NumberDigit[k, -2]]<>"_za_"<>ToString[NumberDigit[za,0]]<>"_"<>ToString[NumberDigit[za,-1]]<>"_"<>ToString[NumberDigit[za,-2]]<>".txt";
awinspoints = ToExpression[StringSplit[Import[path<>"a_wins_points_k_"<>suffix],"\n"]] ;
bwinspoints =ToExpression[StringSplit[ Import[path<>"b_wins_points_k_"<>suffix],"\n"]];
deadlockpoints =ToExpression[StringSplit[Import[path<>"deadlock_points_k_"<>suffix],"\n"]];
convergenceTime = ToExpression[Import[path<>"convergence_time_k_"<>suffix]];
convergenceTimeList = ToExpression[StringSplit[Import[path<>"convergence_times_k_"<>suffix], "\n"]];

(*awinspoints = ToExpression[StringSplit[Import[path<>"a_wins _points _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"],"\n "]] ;
bwinspoints =ToExpression[StringSplit[ Import[path<>"b_wins _points _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"],"\n "]];
deadlockpoints =ToExpression[StringSplit[Import[path<>"deadlock_points _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"],"\n "]];
convergenceTime = ToExpression[Import[path<>"convergence_time _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"]];
convergenceTimeList = ToExpression[StringSplit[Import[path<>"convergence_times _k _ "<>ToString[NumberDigit[k, 0]]<>"_ "<>ToString[NumberDigit[k, -1]]<>"_za _ "<>ToString[NumberDigit[za,0]]<>"_ "<>ToString[NumberDigit[za,-1]]<>"_ "<>ToString[NumberDigit[za,-2]]<>" . txt"], "\n "]];
*)
nPoints=Length[awinspoints]+Length[bwinspoints]+Length[deadlockpoints];
accuracy = Length[awinspoints]/nPoints;
inaccuracy = Length[bwinspoints]/nPoints;
indecision = Length[deadlockpoints]/nPoints;
regret =0;
Do[
regret+=1-bwinspoints[[i]][[2]];
,{i, 1, Length[bwinspoints]}];
Do[
regret+=1;
,{i, 1, Length[deadlockpoints]}];
regret = regret / (Length[awinspoints]+Length[bwinspoints]+Length[deadlockpoints]);
std = PlusMinus[convergenceTime, StandardDeviation[convergenceTimeList]];
AppendTo[accuracies, {k, accuracy}];
AppendTo[regrets, {k,regret}];
AppendTo[inaccuracies, {k,inaccuracy}];
AppendTo[indecisions, {k,indecision}];
AppendTo[stds, {k,std}];
updatedTimeList={};
averageConvergenceTime = 0;
deltaT = 1000;
counter=0;
(*Do[
If[convergenceTimeList[[j]][[3]]>=deltaT, AppendTo[updatedTimeList, {convergenceTimeList[[j]][[1]], convergenceTimeList[[j]][[2]], deltaT}]; averageConvergenceTime+=deltaT, AppendTo[updatedTimeList, convergenceTimeList[[j]]]; averageConvergenceTime+=convergenceTimeList[[j]][[3]]];
,{j, 1, Length[convergenceTimeList]}];
AppendTo[convergenceTimes, {k,averageConvergenceTime/Length[convergenceTimeList]}];
AppendTo[convergenceTimesLists, updatedTimeList];*)

Do[If[convergenceTimeList[[j]][[3]]<deltaT, averageConvergenceTime+=convergenceTimeList[[j]][[3]];counter+=1;];
,{j, 1, Length[convergenceTimeList]}];
AppendTo[convergenceTimesLists,Cases[convergenceTimeList, {_, _, x_} /; x <deltaT]];
AppendTo[convergenceTimes, {k,averageConvergenceTime/counter}];
, {k,kRange[[1]],kRange[[2]],kRange[[3]]}];
{accuracies, regrets, convergenceTimes, stds, convergenceTimesLists, inaccuracies, indecisions}
 ]


MinDistance[costAccuracyList_, point_:{0,0.5}]:=Module[{}, 
minDist = +Infinity;
minDistPoint = {0,0};
Do[dist = EuclideanDistance[point, costAccuracyList[[i,1]]];
If[dist<minDist, minDist=dist; minDistPoint = costAccuracyList[[i]]]
,{i, 1,Length[costAccuracyList]}]
Print[minDist];
Print[minDistPoint];]


GetAveragedMetrics[za_, path1_, path3_, n_, G_]:=Module[
{accuracies = {},
regrets = {},
convergenceTimes = {}, 
costs={},
convergenceTimeList={},
stds={}},
m1 = ExtractK[za, path1];
(*m3 = ExtractK[za, path3];*)
Do[
a1 = m1[[1]][[k*20+1]][[2]];
(*a3 = m3[[1]][[k*20+1]][[2]];*)
(*a = (a1+a3)/2;*)
a=a1;
(*r = (m1[[2]][[k*20+1]][[2]]+m3[[2]][[k*20+1]][[2]])/2;*)
r=m1[[2]][[k*20+1]][[2]];
timeList1 = m1[[5]][[k*20+1]];
(*timeList3 = m3[[5]][[k*20+1]];*)
std = StandardDeviation[timeList1[[All,3]]];
pureTimeList = {};
(*Do[
AppendTo[pureTimeList, (timeList1[[j]][[3]]+timeList3[[j]][[3]])/2];
,
{j, 1, Length[timeList1]}];
timeList = Join[timeList1, timeList3];
onlyTimeList = timeList[[All,3]];
ct = Total[onlyTimeList]/Length[onlyTimeList];*)
ct = Total[timeList1[[All,3]]]/Length[timeList1];
cost=m1[[3]][[k*20+1]]*(k+(1-k)*G);
AppendTo[accuracies, {k, a}];
AppendTo[regrets, {k,r}];
AppendTo[convergenceTimes, {k, ct}];
AppendTo[costs, cost];
AppendTo[convergenceTimeList, timeList1];
AppendTo[stds, {k,PlusMinus[ct, std]}]
,{k,0,1,0.05}];
{accuracies, regrets, convergenceTimes, costs, convergenceTimeList, stds}
]


End[];


EndPackage[];

