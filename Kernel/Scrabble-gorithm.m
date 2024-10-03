BeginPackage["TheDiractionary`Version1`", {"TheDiractionary`ScrabbleScore`"}];
updateUsedCount;
reduceRemainingCount;
RunVersion1Scrabblegorithm;
RunVersion2Scrabblegorithm;
createInitialScrabbleBoard;
updateScrabbleBoard;

Begin["`Private`"]

updateUsedCount[usedCounts_, word_] :=
 Module[{charCounts},
  charCounts = Counts[Characters[word]];
  Merge[{usedCounts, charCounts}, Total]
  ]

reduceRemainingCount[remainingCounts_, word_, blanks_] :=
 	Module[{tiles = Characters[word], charCounts, newCounts, 
   blanksNeeded},
  	    charCounts = Counts[Characters[word]];
  	    newCounts = 
   Merge[{remainingCounts, 
     Map[(# -> (remainingCounts[#] - charCounts[#])) &, tiles]}, Last];
  
  blanksNeeded = Count[Values[newCounts], x_ /; x < 0];
  	   
  	    If[
   blanksNeeded <= blanks && Min[Values[newCounts]] >= -blanks,
   		newCounts["?"] = newCounts["?"] - blanksNeeded;
   	        Return[newCounts],
   	    Return[remainingCounts]
   	]
  ]

RunVersion1Scrabblegorithm[iterations_] := 
  Module[{j = 1, dict, wordsByLength, remainingCounts, bingos, blanks,
     b, word, newRemainingCounts, negativeKeys, i},
   Do[
    Print["\n ...\n ...\n ..."];
    Print[StringJoin["Attempt ", ToString[j]]];
    j++;
    (*Initialize variables*)
    dict = RandomSample[Import["CSW21.txt", "List"]];
    wordsByLength = GroupBy[dict, StringLength];
    remainingCounts = tiles[[All, "Quantity"]];
    bingos = {};
    blanks = {};
    i = 1;
    (*Main loop.*)
    While[i <= Length[wordsByLength[7]], 
     word = wordsByLength[7][[i]];
     If[Length[bingos] < 12, b = 0, b = 1];
     newRemainingCounts = 
      reduceRemainingCount[remainingCounts, word, b];
     If[newRemainingCounts === remainingCounts, i++, 
      negativeKeys = 
       Select[Keys[newRemainingCounts], newRemainingCounts[#] < 0 &];
      If[negativeKeys =!= {}, AppendTo[blanks, negativeKeys[[1]]];
       newRemainingCounts[negativeKeys[[1]]] = 0;];
      AppendTo[bingos, word];
      remainingCounts = newRemainingCounts;
      Print[bingos];
      Print["Tiles Left: ", 
       Length[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]],
        " ", StringJoin[
        Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]]];
      If[Length[bingos] == 14, Print["Success!"];
       CloudPut[
        Append[CloudGet["V.1-WordMaster"], <|"Bingos" -> bingos, 
          "Blanks" -> blanks, 
          "Tiles Remaining" -> 
           Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]|>], 
        "V.1-WordMaster"]];
      If[Length[bingos] == 12, i = 1, i++]];], {iterations}]];



RunVersion2Scrabblegorithm[iterations_] :=
 Module[
    {j, dict, wordsByLength, remainingCounts, usedCounts, bingos, blanks, overlaps, blankTileList, usedWords, i,
   word, b, overlapOptions, overlapTile, newRemainingCounts, charsToDelete},
  	Do[
   		Print["\n ...\n ...\n ..."];
   		Print[StringJoin["Attempt ", ToString[j]]];
   		j++;
   		(* Initialize Variables. *)
   		dict = RandomSample[Import["CSW21.txt", "List"]];
   		wordsByLength = GroupBy[dict, StringLength];
   		remainingCounts = tiles[[All, "Quantity"]];
   		usedCounts = AssociationThread[Keys[tiles], Table[0, 27]];
   		bingos = {};
   		blanks = {};
   		overlaps = {};
   		blankTileList = {};
   		usedWords = <||>;(* Track used words *)
   		i = 1;
   			(* Select Starting Word. *)
   			word = RandomChoice[wordsByLength[7]];
   			remainingCounts = 
    reduceRemainingCount[remainingCounts, word, 0]
   			(* If Starting Word requires blanks, skip to next iteration. *);
   			If[remainingCounts === tiles[[All, "Quantity"]],
    				Print[word];
    				Print["Choose another Starter"];
    				Continue[];
    			];
   			usedCounts = updateUsedCount[usedCounts, word];
   			AppendTo[bingos, word];
   			Print["Bingos: ", bingos];
   			Print["Tiles Left: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]], 
    " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]]];
   			Print["Tiles Used: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]], " ", 
    StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]]];
   			(* Main loop *)
   			While[i <= Length[wordsByLength[8]],
    				word = wordsByLength[8][[i]];
    				If[KeyExistsQ[usedWords, word],
     					i++;
     					Continue[];
     				];
    				If[Length[bingos] < 12,
     					b = 0,
     					b = 1
     				];
    				overlapOptions = Intersection[Characters[word], Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]];
    				If[overlapOptions === {},
     					Print[word, " Has No Overlaps!"];
     					 i++;
     					 Continue[];
     				];
    				overlapTile = RandomChoice[overlapOptions];
    				newRemainingCounts = reduceRemainingCount[remainingCounts, StringJoin[DeleteElements[Characters[word], 1 -> {overlapTile}]], b];
    				If[newRemainingCounts === remainingCounts,
     					i++,
              blankTileList = Select[Keys[newRemainingCounts], newRemainingCounts[#] < 0 &];
     					If[blankTileList =!= {},
      						AppendTo[blanks, blankTileList[[1]]];
      						newRemainingCounts[blankTileList[[1]]] = 0;
      						usedCounts["?"]++;
                  Print[word, " played through: ", overlapTile, " with a blank ", blankTileList[[1]]];,
      						Print[word, " played through: ", overlapTile]
      					];
     					AppendTo[bingos, word];
     					AppendTo[overlaps, overlapTile];
     					charsToDelete = Join[{overlapTile}, blankTileList];
              usedCounts = updateUsedCount[usedCounts, StringJoin[DeleteElements[Characters[word], 1 -> charsToDelete]]];
     					remainingCounts = newRemainingCounts;
     					Print[bingos];
     				  Print["Tiles Left: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]], " ", 
     StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]]];
     					Print["Tiles Used: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]], " ", 
      StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]]];
     		      usedWords[word] = True;(* Mark the word as used *)
     					If[Length[bingos] == 14,
      						Print["Success!"];
      						CloudPut[Append[CloudGet["V.2-WordMaster"],
        							<|"Bingos" -> bingos, "Overlaps" -> overlaps, "Blanks" -> blanks,
         							"Tiles Remaining" -> Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]|>],
       								"V.2-WordMaster"
       							];
      						Break[];
      					];
     					If[Length[bingos] == Or[12, 13],
      						i = 1,
      						i++
      					];
     				];
    			],
   		{j, 1, iterations}
   		]
  	]

createInitialScrabbleBoard[] := 
Module[
    {board, colors},
    board = {{"TW", "SL", "SL", "DL", "SL", "SL", "SL", "TW", "SL", 
     "SL", "SL", "DL", "SL", "SL", "TW"}, {"SL", "DW", "SL", "SL", 
     "SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "DW", 
     "SL"}, {"SL", "SL", "DW", "SL", "SL", "SL", "DL", "SL", "DL", 
     "SL", "SL", "SL", "DW", "SL", "SL"}, {"DL", "SL", "SL", "DW", 
     "SL", "SL", "SL", "DL", "SL", "SL", "SL", "DW", "SL", "SL", 
     "DL"}, {"SL", "SL", "SL", "SL", "DW", "SL", "SL", "SL", "SL", 
     "SL", "DW", "SL", "SL", "SL", "SL"}, {"SL", "TL", "SL", "SL", 
     "SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "TL", 
     "SL"}, {"SL", "SL", "DL", "SL", "SL", "SL", "DL", "SL", "DL", 
     "SL", "SL", "SL", "DL", "SL", "SL"}, {"TW", "SL", "SL", "DL", 
     "SL", "SL", "SL", "DW", "SL", "SL", "SL", "DL", "SL", "SL", 
     "TW"}, {"SL", "SL", "DL", "SL", "SL", "SL", "DL", "SL", "DL", 
     "SL", "SL", "SL", "DL", "SL", "SL"}, {"SL", "TL", "SL", "SL", 
     "SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "TL", 
     "SL"}, {"SL", "SL", "SL", "SL", "DW", "SL", "SL", "SL", "SL", 
     "SL", "DW", "SL", "SL", "SL", "SL"}, {"DL", "SL", "SL", "DW", 
     "SL", "SL", "SL", "DL", "SL", "SL", "SL", "DW", "SL", "SL", 
     "DL"}, {"SL", "SL", "DW", "SL", "SL", "SL", "DL", "SL", "DL", 
     "SL", "SL", "SL", "DW", "SL", "SL"}, {"SL", "DW", "SL", "SL", 
     "SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "DW", 
     "SL"}, {"TW", "SL", "SL", "DL", "SL", "SL", "SL", "TW", "SL", 
     "SL", "SL", "DL", "SL", "SL", "TW"}};
    colors = {
        "TW" -> Red, "SL" -> Darker[Green], "DL" -> Cyan,
        "TL" -> Blue, "DW" -> Orange
      };
    ArrayPlot[board, ColorRules -> colors, Mesh -> True, 
   MeshStyle -> Black]
  ]

updateScrabbleBoard[word_, pos_, direction_ : ("\[RightArrow]" | "\[DownArrow]"), epilogState_] :=
   Module[{x, y, dx, dy, length, epilog},
      length = StringLength[word];
      x = (ToExpression[StringTake[pos, 2 ;;]] - 1);
      y = 15 - (LetterNumber[StringTake[pos, 1]] - 1);
      If[direction === "\[RightArrow]",
   epilog = Table[
           {
              {LightYellow, EdgeForm[Thin], 
       Rectangle[{x + (i - 1), y}, {x + i, y - 1}]},
              Text[Style[Characters[word][[i]], 20],
                {x + (i - 0.5), y - 0.5}]
            },
           {i, length}
         ],
   epilog = Table[
           {
              {LightYellow, EdgeForm[Thin], 
       Rectangle[{x, y - (i - 1)}, {x + 1, y - i}]},
              Text[Style[Characters[word][[i]], 20],
                {x + 0.5, y - (i - 0.5)}]
            },
           {i, length}
         ]
   ];
      Join[epilog, epilogState]
    ]
End[]

EndPackage[]