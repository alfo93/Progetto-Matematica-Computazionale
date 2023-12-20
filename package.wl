(* ::Package:: *)

BeginPackage["Limiti`"];

(* Export section *)
ShowDiscontinuities ::usage = "Funzione che mostra i tre tipi di punti di discontinuit\[AGrave] di funzione"
FunctionFromString ::usage = "Prende in input una funzione scritta in linguaggio naturale e la restituisce come oggetto di Mathematica";
DrawPolynomial ::usage = "Disegno di polinomi random con relativi grafici"
MakeRationalPolynomial ::usage = "Generazione di polinomio random razionale"
DrawRandomPoly ::usage = "Creazione di un polinomio di grado 1 random con relativo grafico"
CreateDomain ::usage = "Restituisce una lista di tutti i valori del dominio di f"
AnimateAPoint ::usage = "Animazione di un punto su una funzione"
AnimateTwoPoint ::usage = "Animazione di due punti su una funzione"
AnimateDiscontinuities ::usage "Visualizzazione di punti dinamici su funzioni"
Esercizio1 ::usage = "Esercizi su P.Discontinuit\[AGrave] e calcolo di limiti di funzioni discontinue"
Esercizio2 ::usage = "Esercizi su P.Discontinuit\[AGrave] interittivi"
SetSeed ::usage = "Funzione che mostra il form per l'inserimento del seed per la generazione di codice pseudorandom"


Begin["Private`"];
(*
	Funzione che permette di vedere tre funzioni con i tre tipo di punti di discontinuit\[AGrave] 
*) 
ShowDiscontinuities[Tipo_] :=
	Grid[{{Text[Style["I tre tipi di discontinuit\[AGrave]", FontSize->Large]]},{
	(* Viene creata un'interfaccia, attraverso una manipulate, per scegliere la discontinuit\[AGrave] che si vuole visualizzare *)
	Manipulate[Switch[Tipo, (* Il valore del men\[UGrave] viene valutato in uno switch per disegnare l'opportuna funzione discontinua *)
					1,ShowDiscontinuity1[], (* Primo tipo di discontinuit\[AGrave], Piecewise[{{x^2-10,x<0},{-x^2+10,x>0}}] *)
					2,ShowDiscontinuity2[], (* Secondo tipo di discontinuit\[AGrave], (1/x) *)
					3,ShowDiscontinuity3[]] (* Secondo tipo di discontinuit\[AGrave], ((1-x^2)/(x-1)) *)
				,{Tipo,Range[1,3,1]} (*drop-down menu*)]}}]

(*
	Funzione che trasforma una stringa in una funzione matematica
	(str,vars) -> (function) con 
		str : stringa in cui \[EGrave] salvata la funzione da convertire
		vars : simboli della funzione 
*)
FunctionFromString[str_, vars_] := 
	Function[Evaluate[ToExpression[Characters[vars],TraditionalForm]], 
	Evaluate[ToExpression[str,TraditionalForm]]]
	
(*  
	Funzione restituisce un polinomio razionale generato casualmente.
	
	La funzione chiama la MakeRationalPolynomial2 finch\[EAcute] non viene 
	restituito un polinomio razionale con almeno un punto di discontinuit\[AGrave].
	(n,n2,x) -> (function) con 
		n : grado del numeratore
		n2 : grado del numeratore
		x : simbolo del polinomio
*)	
MakeRationalPolynomial[num_,den_,x_]:=
	Module[{n,f,domain},n=1;
		While[n>0,f=MakeRationalPolynomial2[num,den,x]; (* Viene richiamata MakeRationalPolynomial2 finch\[EGrave] il polinomio razionale restituito non ha almeno un punto di discontinuit\[AGrave]*)
	       domain=CreateDomain[f,x]; (* I punti di discontinuit\[AGrave] per il controllo vengono restituiti dalla funzione CreateDomain del package*)
	       If[Length[domain]>0,n=0;,n=1;];]; (* Se si trova almeno una discontinuit\[AGrave] si mette n a 1 in modo che l'espressione nella guardia del while non risulti pi\[UGrave] vera *)
	    f]; (* La f finale create da MakeRationalPolynomial2 *)

(*  
	Funzione di supporto per Make Rational Polynomial .
	
	Restituisce un polinomio razionale generato casualmente.
	Essendo generato casualmente, \[EGrave] possibile che il denominatore non
	abbia soluzione reali -> niente punti di discontinuit\[AGrave].
	(n,n2,x) -> (function) con 
		n : grado del numeratore
		n2 : grado del numeratore
		x : simbolo del polinomio
*)		
MakeRationalPolynomial2[n_,n2_,x_] := 
	Module[{num,den,f},
		(* La polinomiale razionale viene creata unendo due polinomiali,
			una al numeratore e l'altra al denominatore
		*) 
		num=MakePolynomial[n,x]; (* Polinomiale da mettere al numeratore *)
		den=MakePolynomial[n2,x]; (* Polinomiale da mettere al denumeratore *)
		Evaluate[num]/Evaluate[den]
		]
		
(*  
	Funzione che restituisce un polinomio generato casualmente
	(n,x) -> (expr) con 
		n_Integer : grado del polinomio
		x_Symbol : simbolo polinomio 
*)
MakePolynomial[n_Integer,x_Symbol] := Module[{z,c}, z = RandomChoice[{-1,1}] RandomInteger[{1,10}];
	c = Table[RandomInteger[{-10,10}], {n}];
	FromDigits[Reverse[AppendTo[c,z]],x]]
	
(*
	La funzione disegna un polinomio razionale randomico inserendo 
	il grado del numeratore e del denominatore attraverso due slider.
*)	
DrawPolynomial[x_] := 
	Manipulate[doit;  (* La manipulate viene usata per avere gli slider che controllano il grado del numeratore e del 
						 denominatore *)
		Module[{f}, (* f : funzione polinomio razionale *)
		f = MakeRationalPolynomial[n,n2,x]; (* In f viene restituita una funzione polinomio razionale *)
		Row[{ f //DisplayForm , (* Plot di f, viene usato il parametro PerformanceGoal -> "Quality" per evitare che la funzione venga ridisegnata due volte ad ogni modifica degli slider *)
		Plot[f,{x,-Infinity,Infinity},PerformanceGoal -> "Quality", ImageSize->500,PlotRange->{{-Infinity,Infinity},{-Infinity,Infinity}}, ScalingFunctions->"Infinite"]}, "="]
		], (*End module*)
		{{n,1,"Max Polynomial order num"},1,10,1,Appearance->"Labeled"}, (* Slider della manipulate usato per il grado del numeratore *)
		{{n2,1,"Max Polynomial order den"},1,10,1,Appearance->"Labeled"}, (* Slider della manipulate usato per il grado del denumeratore *)
		Button["Nuova funzione",doit=Date[],ImageSize->150],ContentSize->{850,450}]
		
(*
	Funzione che disegna un polinomio razionale di grado 1 generato casualmente con MakeRationalPolynomial
	Viene anche calcolato il limite destro e sinistro del punto di discontinuit\[AGrave].
*)
DrawRandomPoly[x_] := 
	Module[{p},
	p = MakeRationalPolynomial[0,1,x]; (* Generazione polinomio razionale di grado 1 *)
	Grid[{
		{Row[ (* Riga in cui viene fatto il plot della funzione p  *)
			{"\[Florin](\[FormalX]) ", p //DisplayForm, Plot[p,{x,-15,15},ImageSize->600, Exclusions->Last[FunctionDiscontinuities[p,x]],Epilog->{PointSize[Large],Point[{Last[FunctionDiscontinuities[p,x]],0}]}]},"\[DoubleLongRightArrow]"]
		},
		{Row[ (* Limite sinistro di p in \[Alpha]   *)
			{"Limite Sinistro: \!\(\*UnderscriptBox[\(\[Limit]\), \(\[FormalX]\[LongRightArrow]\[Alpha]-\)]\) \[Florin](\[FormalX])",Limit[p,x->Last[FunctionDiscontinuities[p,x]],Direction->1]},"="]
		},
		{Row[ (* Limite destro di p in \[Alpha]   *)
			{"Limite Destro: \!\(\*UnderscriptBox[\(\[Limit]\), \(\[FormalX]\[LongRightArrow]\[Alpha]+\)]\) \[Florin](\[FormalX])",Limit[p,x->Last[FunctionDiscontinuities[p,x]],Direction->-1]},"="]
		}
		}]
	]
	
	
(* 
	Data una funzione f viene restituito un insieme di punti corrispondenti ai valori di discontinuit\[AGrave] del dominio 
		f : funzione su cui calcolare il dominio
		x : simbolo in cui valutare la funzione
*)
CreateDomain[f_,x_] := Module[{pdisc,pdiscList}, 
							pdisc = Solve[FunctionSingularities[f,x],Reals];
							pdiscList = Table[Last[Last[pdisc[[i]]]],{i,Length[pdisc]}]
						]

	
CreateDiscontinuitiesPoly[x_]:=Module[{n,f,domain},n=1;
	While[n>0,f=MakeRationalPolynomial[2,2,x];
		domain=CreateDomain[f,x];
		If[Length[domain]>0,n=0;,n=1;];];
		f];
	

(* 
	Funzione che mostra una funzione con punto di discontinuit\[AGrave] d grado 1
	La discontinuit\[AGrave] viene disegnata in rosso tratteggiato
*)
ShowDiscontinuity1[]:=
	Block[{f, discp},
	f=  Piecewise[{{x^2-10,x<0},{-x^2+10,x>0}}]; (* Funzione da disegnare *)
	discp =  FunctionDiscontinuities[f,x];
	line= Line[{{Last[discp], 10}, {Last[discp], -10}}];
	Grid[{{Plot[{f,line}, {x, -5,5},ImageSize->600,Epilog->{Red,Dashed,line,PointSize[Medium],Point[{Last[discp],0}]}, 
	PlotStyle->{Thick,Dashed}, Exclusions->{Last[discp]}]},{OpenerView[{"Discontinuit\[AGrave] di primo tipo"," - I limiti destro e sinistro esistono, ma hanno valori diversi"}]}}]
]

(* 
	Funzione che mostra una funzione con punto di discontinuit\[AGrave] di grado 2
	La discontinuit\[AGrave] viene disegnata in rosso tratteggiato
*)
ShowDiscontinuity2[]:=
	Block[{f, discp},
	f= (1/x); (* Funzione da disegnare *)
	discp =  FunctionDiscontinuities[f,x];
	line= Line[{{Last[discp], 10}, {Last[discp], -10}}];
	Grid[{{Plot[{f,line}, {x, -5,5},ImageSize->600,Epilog->{Red,Dashed,line,PointSize[Medium],Point[{Last[discp],0}]}, 
	PlotStyle->{Thick,Dashed}, Exclusions->{Last[discp]}]},{OpenerView[{"Discontinuit\[AGrave] di secondo tipo"," - Almeno uno dei due limiti destro o sinistro \[EGrave] infinito o non esiste"}]}}]
]

(* 
	Funzione che mostra una funzione con punto di discontinuit\[AGrave] di grado 3
	La discontinuit\[AGrave] viene disegnata in rosso tratteggiato
*)
ShowDiscontinuity3[]:=
	Block[{f, discp},
	f = ((1-x^2)/(x-1)); (* Funzione da disegnare *)
	discp =  FunctionDiscontinuities[f,x];
	line= Line[{{Last[discp], 10}, {Last[discp], -10}}];
	Grid[{{Plot[{f,line}, {x, -2,4},ImageSize->600,Epilog->{Red,Dashed,line,PointSize[Medium],Point[{Last[discp],Limit[f,x->Last[discp],Direction->-1]}]}, 
	PlotStyle->{Thick,Dashed}, Exclusions->{Last[discp]}]},{OpenerView[{"Discontinuit\[AGrave] di terzo tipo"," - Esiste ed \[EGrave] finito il limite di F(x) per x->x0 \n - F non \[EGrave] definita in x0 \n Questo tipo di discontinuit\[AGrave] \[EGrave] definita discontinuit\[AGrave] apparente"}]}}]
]

(*
	Animazione di un punto che si muove lungo una funzione
	Usata in Limiti con Polinomi
*)			
AnimateAPoint[] :=
		Block[{},
			s[t_] := 9 / (1-7t);
			pt[t_] := Graphics[{Red, AbsolutePointSize[8], Point[{t, s[t]}]}];
			Manipulate[ 
               Show[plot, pt[t]],
                {{t, -10,"Punto "}, -10, 10, .1, AppearanceElements -> All,ControlType->Animator},
                {{plot, Plot[s[t], {t, -10, 10}, ImageSize -> 5 72, Exclusions->Last[FunctionDiscontinuities[s[t],t]]]}, None}]
            ]	
	
(* 
	Crea una animazione con due punti che si muovono dagli estremi opposti lungo f per arrivare in i 
		fn : funzione da disegnare
		size : intero usato per le dimensioni del grafico
		i : valore dell'ascissa verso cui si muovono i punti animati
	Usata per muovere due punti verso la discontinuit\[AGrave]	
*)

AnimateTwoPoint[fn_,size_,i_]:=
	Grid[{{
	Manipulate[
		Show[	
			Plot[fn[x],{x,i-size,i+size}],
			Graphics[{Red,AbsolutePointSize[10],Point[{pt1,fn[pt1]}]}], (* Punto che si muove da sinistra *)
			Graphics[{Red,AbsolutePointSize[10],Point[{pt2,fn[pt2]}]}] (* Punto che si muove da destra *) 
		], (* Il valore x dei punti \[EGrave] gestito da slider della manipulate, la y \[EGrave] calcolata con fn *)
	{pt1,i-size,i-0.01,.05,ControlType->Animator},{pt2,i+size,i,0.01,ControlType->Animator}, Text[StringJoin["Limite Sx: ",ToString[Limit[fn[x],x->i,Direction->1]]]], Text[StringJoin["Limite Dx: ",ToString[Limit[fn[x],x->i,Direction->-1]]]]]}}]
	(* ControlType->Animator usato per rendere automatica l'animazione della manipulate, sostituisce i suoi controlli con quelli della animate *)
		

(*
	Funzione che crea un'animazione per il limite sinistro
	(fn,i,{xmin,xmax},{submin,submax}) con 
		fn : funziona da visualizzare
		i : punti di discontinuit\[AGrave]
		{xmin,xmax} : Dimensione della plot
		{submin,submax} : Estremi per l'animazione del punto lungo la funzione
*)
AnimateLimitL[fn_,i_,{xmin_,xmax_},{submin_,submax_}]:= 
					Button[StringJoin[ToString[i]," Sx"],
					CreateDialog[
							Manipulate[
								Show[
									Plot[fn[x],{x,xmin,i},ImageSize->800],
										Graphics[{Red,AbsolutePointSize[8],Point[{range,fn[range]}]}]],
										{{range,xmin,"Range"},xmin,submax,.01}]]]
										
(*
	Funzione che crea un'animazione per il limite sinistro
	(fn,i,{xmin,xmax},{submin,submax}) con 
		fn : funziona da visualizzare
		i : punto di discontinuit\[AGrave]
		{xmin,xmax} : Dimensione della plot
		{submin,submax} : Estremi per l'animazione del punto lungo la funzione
*)
AnimateLimitR[fn_,i_,{xmin_,xmax_},{submax_,submin_}]:= 
					Button[StringJoin[ToString[i]," Dx"],
					CreateDialog[
							Manipulate[
								Show[
									Plot[fn[x],{x,i,xmax},ImageSize->800],
										Graphics[{Red,AbsolutePointSize[8],Point[{range,fn[range]}]}]],
										{{range,xmax,"Range"},xmax,submax,.01}]]]
(*
	Per un punto di discontinuit\[AGrave] di f crea una coppia di bottoni che chiamano AnimateLimitR e AnimateLimitL
		fn : funziona che ci interessa
		i : punto di discontinuit\[AGrave]
		{xmin,xmax} : Dimensione della plot
		{submin,submax} : Estremi per l'animazione del punto lungo la funzione
*)
AnimateLimit[fn_,{xmin_,xmax_},subranges:{{_,_}...},i_]:= Row[If[i < Last[#],AnimateLimitR[fn,i,{xmin,xmax},#],AnimateLimitL[fn,i,{xmin,xmax},#]]&/@subranges]
	
(*
	Funzione che si occupa di mostrare delle animazione per mostrare il calcolo di un limite.
	
	L'animazione consiste in un movimento di un punto lungo una funzione 
	fino a quando non raggiunge il punto in cui viene calcolato il limite.
	
	Data una funzione f e una lista pts di suoi punti di discontinuit\[AGrave], 
	per ogni punto p di pts viene creata una coppia di pulsanti, chiamando AnimateLimit, 
	che permettono di visualizzare un'animazione del limite per quei punti 
		f : funziona che ci interessa
		pts : punti di discontinuit\[AGrave]
		size : valore intero usato per le dimensioni della plot
*)
AnimateDiscontinuities[f_,pts_,size_Integer]:=
	Table[AnimateLimit[f,{-size,size},{{i-size,i},{i,size+i}},i],{i,pts}]
	

(*
	Mostra all'utente un esercizio sui limiti.
		- Generazione funzione discontinua random
		- Plot della funzione 
		- Calcolo dei punti di discontinuit\[AGrave]
		- Calcolo limiti sui punti di discontinuit\[AGrave]
*)
Esercizio1[x_]:=
	Grid[{
		{y=".."; plot="..";pDisc ="..";
		b1=Button["Genera funzione polinomiale ",y=CreateDiscontinuitiesPoly[x],Method->"Queued "],
		b2=Button["Plotta Funzione ",plot = Plot[y,{x, -10, 10},ImageSize->400] ],
		b3=Button["Calcola P . Discontinuit\[AGrave]",pDisc=Solve[FunctionSingularities[y,x]]],
		b4 = Button["Reset ",y=".."; plot="..";pDisc =".."]
	},
	{ Text[Style[Dynamic@y,FontSize->24]],Dynamic@plot ,Text[Style[Dynamic@pDisc, FontSize->24]]},
		{Button["Calcola limiti ",
			NotebookLocate["Limite"]; NotebookDelete[];
			For[i=1, i<=Length[pDisc],i++, 
			CellPrint[{TextCell[Row[{"Limite di: ",y, " con x che tende a: ", Last[Last[pDisc[[i]]]],  "  da dx->sx  = ", Limit[Evaluate[y], x->Last[Last[pDisc[[i]]]],Direction->-1], "  E da sx->dx  =  ", Limit[Evaluate[y], x->Last[Last[pDisc[[i]]]], Direction->+1]}]
			,"Output", CellTags->"Limite"]}]
		]],
		Button["Cancella limiti", NotebookLocate["Limite"]; NotebookDelete[];]}
	}]	
	
Esercizio2[x_] := 
	Grid[{
		{Print["Inserisci una funzione: "]},
		{InputField[Dynamic[string],String,FieldHint->"Inserisci una funzione discontinua",FieldSize->Medium]},
		{Row[{
			Button["Visualizza graficamente la funzione ",NotebookLocate["plotFuncEs2"];NotebookDelete[];CellPrint[TextCell[Row[{Plot[Evaluate[ToExpression[string,TraditionalForm]],{x,-10,10}]}],"Output",CellTags->"plotFuncEs2"]]],
			Button["Reset grafico",NotebookLocate["plotFuncEs2"];NotebookDelete[]]}
		]," "},
		{},
		{Row[{InputField[Dynamic[stringDisc],String,FieldHint->"Inserisci le discontinuit\[AGrave] della funzione"],
								Button["Valuta la soluzione",Block[{pdisc,sd=stringDisc,s=string},pdisc = CreateDomain[FunctionFromString[s,"x"],x]; 
															Table[If[ToString[pdisc[[i]]] == sd,
															CellPrint[TextCell[Row[{Style["Bene, il risultato \[EGrave] corretto. Ottimo lavoro!",Green]}],"Output",CellTags->"okFunc"]],
															CellPrint[TextCell[Row[{Style["Valore Errato, ritenta l'esercizio e correggi gli errori", Red]}],"Output",CellTags->"noFunc"]]],
															{i,Length[pdisc]}]];]}]," "},
		{},
		{Button["Reset Esercizio",NotebookLocate["okFunc"];NotebookDelete[];NotebookLocate["noFunc"];NotebookDelete[];NotebookLocate["plotFuncEs2"];NotebookDelete[]]}
	}]
	
SetSeed[] :=
	 Grid[{
	        {Print["Inserire qui il valore del seed desiderato"]},
			{Row[{
				InputField[Dynamic[seedInt],String,FieldHint->"Inserire il seed desiderato"],Button[Style["Esegui seed",Bold,Darker@Red],SeedRandom[seedInt]]},"\[Rule]"]
			}
		}]

End[]; (* `Private` *)
EndPackage[];
