BeginPackage["TheDiractionary`Version1`", {"TheDiractionary`ScrabbleScore`"}];
UpdateUsedTileCount;
UpdateRemainingTileCount;
RunVersion1Scrabblegorithm;
RunVersion2Scrabblegorithm;
CreateInitialScrabbleBoard;
CheckForBlanksUsed;
UpdateScrabbleBoard;
PositionReformat;
FindPossibleOverlapPositions;
FindPossibleOverlapEpilogs;


Begin["`Private`"]

UpdateUsedTileCount[usedCounts_, word_] :=
 Module[{charCounts},
  charCounts = Counts[Characters[word]];
  Merge[{usedCounts, charCounts}, Total]
  ]

UpdateRemainingTileCount[remainingCounts_, word_, blanks_] :=
 	Module[{tiles = Characters[word], charCounts, newCounts, blanksNeeded},
  	    charCounts = Counts[Characters[word]];
  	    newCounts = Merge[{remainingCounts, Map[(# -> (remainingCounts[#] - charCounts[#])) &, tiles]}, Last];
  
        blanksNeeded = Count[Values[newCounts], x_ /; x < 0];
  	   
  	    If[blanksNeeded <= blanks && Min[Values[newCounts]] >= -blanks,
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
      UpdateRemainingTileCount[remainingCounts, word, b];
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
   			remainingCounts = UpdateRemainingTileCount[remainingCounts, word, 0]
   			(* If Starting Word requires blanks, skip to next iteration. *);
   			If[remainingCounts === tiles[[All, "Quantity"]],
    				Print[word];
    				Print["Choose another Starter"];
    				Continue[];
    			];
   			usedCounts = UpdateUsedTileCount[usedCounts, word];
   			AppendTo[bingos, word];
   			Print["Bingos: ", bingos];
   			Print["Tiles Left: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]]];
   			Print["Tiles Used: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]]];
   			(* Main loop *)
   			While[i <= Length[wordsByLength[8]],
    				word = wordsByLength[8][[i]];
    				If[KeyExistsQ[usedWords, word], i++; Continue[]];
    				If[Length[bingos] < 12,	b = 0, b = 1];
    				overlapOptions = Intersection[Characters[word], Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]];
    				If[overlapOptions === {},
     					Print[word, " Has No Overlaps!"];
     					 i++;
     					 Continue[];
     				];
    				overlapTile = RandomChoice[overlapOptions];
    				newRemainingCounts = UpdateRemainingTileCount[remainingCounts, StringJoin[DeleteElements[Characters[word], 1 -> {overlapTile}]], b];
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
              usedCounts = UpdateUsedTileCount[usedCounts, StringJoin[DeleteElements[Characters[word], 1 -> charsToDelete]]];
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
  (* Unsure about blankTileList. It exists because it leaves room for there being one blank or zero. *)

(* Create Graphic of an Empty Scrabble Board. *)
CreateInitialScrabbleBoard[] := 
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

(* Create the Epilog of a 'word' played in a position 'pos' in the following 'direction'. *)
UpdateScrabbleBoard[word_, pos_List, direction_String : ("Right" | "Down"), epilogState_] :=
   Module[{x, y, length, epilog},
      length = StringLength[word];
      x = (pos[[2]] - 1);
      y = 15 - (LetterNumber[pos[[1]]] - 1);
      If[direction === "Right",
   epilog = Table[
           {
              {LightYellow, EdgeForm[Thin], 
       Rectangle[{x + (i - 1), y}, {x + i, y - 1}, RoundingRadius -> 0.2]},
              Text[Style[Characters[word][[i]], 14],
                {x + (i - 0.5), y - 0.5}]
            },
           {i, length}
         ],
   epilog = Table[
           {
              {LightYellow, EdgeForm[Thin], 
       Rectangle[{x, y - (i - 1)}, {x + 1, y - i}, RoundingRadius -> 0.2]},
              Text[Style[Characters[word][[i]], 14],
                {x + 0.5, y - (i - 0.5)}]
            },
           {i, length}
         ]
   ];
      Join[epilog, epilogState]
    ]

CheckForBlanksUsed[newRemainingCounts_, remainingCounts_, assoc_] := 
 Module[
  {
   negativeKeys = 
    Select[Keys[newRemainingCounts], newRemainingCounts[#] < 0 &],
   blanksUsed,
   index = Length[assoc] + 1
   },
  blanksUsed = If[negativeKeys == {}, 0, First[negativeKeys]];
  AppendTo[assoc, index -> <|"Blank" -> blanksUsed|>];
  newRemainingCounts[blanksUsed] = 0
  ]

PositionReformat[{rows_, columns_}] := Flatten[Outer[List, rows, columns], 1]

(* 
   Takes the current Master Association, the word to be played, and the possible options for the overlap tile,
   and returns an association in the form:
   <| "Down" -> <| "OverlapTileOption" -> {pos1, pos2, ...} |>, "Right" -> ... |>
*)
FindPossibleOverlapPositions[assoc_, word_, overlapTileOptions_List] := 
 Module[{overlapAssoc = <|"Down" -> <||>, "Right" -> <||>|>},
  Do[
   Do[
    Module[
     {direction = Values[assoc][[All, "Direction"]][[i]], (* Find direction of word to be "played through". *)
      boardRow = LetterNumber[Values[assoc][[All, "Position"]][[i]][[1]]], (* Find row of word to be "played through". *)
      boardColumn = Values[assoc][[All, "Position"]][[i]][[2]], (* Find column of word to be "played through". *)
      avoidRows,
      avoidCols,
      positionListDown,
      positionListRight,
      overlapTileWithinPlayedWord = (StringPosition[Values[assoc][[All, "Bingo"]][[i]], overlapTileOptions[[j]]][[All, 1]] - 1), (* Find position(s) within played word where the overlap tile exists. *)
      workBackToStartNewWord = (StringPosition[word, overlapTileOptions[[j]]][[All, 1]] - 1) (* Find position(s) within new word where the overlap tile exists. *)
      },
      If[
        direction == "Right", (* Play "Down" through this word. *)
        avoidCols = Values[Select[assoc, #["Direction"] == "Down" &][[All, "Position"]]][[All, 2]]; (* Do not play "Down" through existing "Down" plays. *)
        positionListDown = Select[
                    PositionReformat[
                          {ToUpperCase[FromLetterNumber[boardRow - workBackToStartNewWord]],
                          boardColumn + overlapTileWithinPlayedWord}
                                  ],
                              FreeQ[avoidCols, #[[2]]] && LetterNumber[#[[1]]] <= 8 & (* Condition for playing Down. *)
                              ];
        
          If[positionListDown =!= {}, AppendTo[overlapAssoc["Down"], overlapTileOptions[[j]] -> positionListDown]], (* Do not include empty lists. *)
        
        (* Else, Play "Right" through this word. *)
        avoidRows = Values[Select[assoc, #["Direction"] == "Right" &][[All, "Position"]]][[All, 1]]; (* Do not play "Right" through existing "Right" plays. *)
        positionListRight = Select[
                      PositionReformat[
                            {ToUpperCase[FromLetterNumber[boardRow + overlapTileWithinPlayedWord]],
                          boardColumn - workBackToStartNewWord}
                                    ],
                            FreeQ[avoidRows, #[[1]]] && #[[2]] > 0 && #[[2]] <= 8 & (* Condition for playing Right. *)
                                ]; 
        
          If[positionListRight =!= {}, AppendTo[overlapAssoc["Right"], overlapTileOptions[[j]] -> positionListRight]]
        ]
     ],
    {i, Length[assoc]}],
   {j, Length[overlapTileOptions]}];
  Select[overlapAssoc, # =!= <||> &] (* After turn 1, the "Down" association is empty. *)
  ]

FindPossibleOverlapEpilogs[word_, overlapPositions_, epilogState_] :=
     Module[{epilogAssoc = <||>, overlapTileOptions = DeleteDuplicates[Flatten[Keys[Values[overlapPositions]]]]},
          Do[
            Do[
              With[{positions = Lookup[overlapPositions, dir, <||>][letter]},
                If[ListQ[positions] && Length[positions] > 0, (* Check if valid plays exist for the letter and direction. *)
                  Do[
                    Module[{newEpilog},
                          newEpilog = epilogState;
                          newEpilog = UpdateScrabbleBoard[word, pos, dir, newEpilog];
                          AppendTo[epilogAssoc, {pos, dir, letter} -> newEpilog]
                          ],
                    {pos, positions}
                    ]
                  ]
                ],
              {dir, Keys[overlapPositions]}
              ],
          {letter, overlapTileOptions}
          ];
      epilogAssoc
      ]

End[]

EndPackage[]