BeginPackage["TheDiractionary`Version1`", {"TheDiractionary`ScrabbleScore`"}];
UpdateUsedTileCount;
UpdateRemainingTileCount;
RunVersion1Scrabblegorithm;
VisualizeVersion1;
RunVersion2Scrabblegorithm;
CreateInitialScrabbleBoard;
UpdateScrabbleBoard;
FindStartingSquares;
FindPossibleOverlapPositions;
ForbiddenSquares;
UpdateForbiddenSquares;
Battleship;
IdentifyBlanks;
FormatWordWithBlanks;
BlanksAllowed;
RunScrabblegorithm;
GameToEpilog;
PerfectScrabbleGameQ;



(*https://www.reddit.com/r/scrabble/comments/my5tie/the_419_words_erased_from_csw/*)

Begin["`Private`"]

(* Updates association containing (Tile -> No. Used) pairs. *)
UpdateUsedTileCount[usedCounts_, word_] :=
 Module[{charCounts},
  charCounts = Counts[Characters[word]];
  Merge[{usedCounts, charCounts}, Total]
  ]

(* Updates association containing (Tile -> No. Remaining) pairs. *)
UpdateRemainingTileCount[remainingCounts_, word_, blanks_] :=
 	Module[{tiles = Characters[word], charCounts, newCounts, blanksNeeded},
  	    charCounts = Counts[Characters[word]];
  	    newCounts = Merge[{remainingCounts, Map[(# -> (remainingCounts[#] - charCounts[#])) &, tiles]}, Last];
        blanksNeeded = Abs[Total[Select[Values[newCounts], # < 0 &]]];
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
    (*Print["\n ...\n ...\n ..."];*)
    Print[StringJoin["Attempt ", ToString[j]]];
    j++;
    (* Initialize variables. *)
    dict = RandomSample[Import["CSW21.txt", "List"]];
    wordsByLength = GroupBy[dict, StringLength];
    remainingCounts = tiles[[All, "Quantity"]];
    bingos = {};
    blanks = {};
    i = 1;
    (* Main loop. *)
    While[i <= Length[wordsByLength[7]], 
     word = wordsByLength[7][[i]];
     If[Length[bingos] < 12, b = 0, b = 1];
     newRemainingCounts = UpdateRemainingTileCount[remainingCounts, word, b];
     If[newRemainingCounts === remainingCounts, If[i == Length[wordsByLength[7]], Print["No Valid Words"]]; i++, 
      negativeKeys = Select[Keys[newRemainingCounts], newRemainingCounts[#] < 0 &];
      If[negativeKeys =!= {}, AppendTo[blanks, negativeKeys[[1]]]; newRemainingCounts[negativeKeys[[1]]] = 0;];
      AppendTo[bingos, word];
      remainingCounts = newRemainingCounts;
      Print["Bingos Found: ", bingos];
      Print["Tiles Left: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]]];
      If[Length[bingos] == 14, Print["Success!"];
       CloudPut[
        Append[CloudGet["V.1-WordMaster"], 
          <|"Bingos" -> bingos, 
          "Blanks" -> blanks, 
          "Tiles Remaining" -> Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]
          |>
          ], 
        "V.1-WordMaster"]
        ];
      If[Length[bingos] == 12, i = 1, i++]
      ];
    ], {iterations}
  ]
];

VisualizeVersion1[game_] :=
 Module[
  {epilog, board = CreateInitialScrabbleBoard[], bingos = game["Bingos"], blanks = game["Blanks"], leftover = game["Tiles Remaining"]},
  (* Place Bingos on board. *)
    epilog = Fold[
        Module[{col, row},
            row = If[#2 <= 7, ToUpperCase[FromLetterNumber[2 #2 - 1]], ToUpperCase[FromLetterNumber[2 (#2 - 7)]]];
            col = If[#2 <= 7, 1, 9];
            UpdateScrabbleBoard[bingos[[#2]], {row, col}, "Right", #]
            ] &, {}, Range[Length[bingos]]
                ];
   (* Replace letters with blanks. *)
    epilog = Module[
      {rEpilog = Partition[epilog, 7], finalTurns},
        finalTurns = {rEpilog[[-2]][[All, 2]][[All, 1]], rEpilog[[-1]][[All, 2]][[All, 1]]};
        rEpilog[[-2, All, 2, 1]] = ReplacePart[finalTurns[[1]], RandomChoice[Position[finalTurns[[1]], blanks[[1]]]] -> "?"];
        rEpilog[[-1, All, 2, 1]] = ReplacePart[finalTurns[[2]], RandomChoice[Position[finalTurns[[2]], blanks[[2]]]] -> "?"];
        rEpilog
        ];
  (* Place leftover tiles in corner. *)
    epilog = Fold[
        UpdateScrabbleBoard[leftover[[#2]], {"O", #2}, "Right", #] &, epilog, Range[2]
                ];
    
    Show[board, ImageSize -> 400, Epilog -> epilog]
    ]

RunVersion2Scrabblegorithm[iterations_] :=
 Module[
    {j, dict, wordsByLength, remainingCounts, usedCounts, bingos, blanks, overlaps, blankTileList, usedWords, i,
   word, b, overlapOptions, overlapTile, newRemainingCounts, charsToDelete},
  	Do[
   		(*Print["\n ...\n ...\n ..."];*)
   		(*Print[StringJoin["Attempt ", ToString[j]]];*)
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
    				If[newRemainingCounts === remainingCounts,  If[i == Length[wordsByLength[8]], Print["No Valid Words"]];
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
     					Print["Bingos: ",bingos];
     				  Print["Tiles Left: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]]];
     					Print["Tiles Used: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]]];
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
              Text[Style[Characters[word][[i]], 16, Bold],
                {x + (i - 0.5), y - 0.5}]
            },
           {i, length}
         ],
   epilog = Table[
           {
              {LightYellow, EdgeForm[Thin], 
       Rectangle[{x, y - (i - 1)}, {x + 1, y - i}, RoundingRadius -> 0.2]},
              Text[Style[Characters[word][[i]], 16, Bold],
                {x + 0.5, y - (i - 0.5)}]
            },
           {i, length}
         ]
   ];
      Join[epilogState, epilog]
    ]


(* Finds list of starting positions of a new bingo. *)
FindStartingSquares[boardRow_Integer, boardCol_Integer, retrace_, toTile_, dirOfPlay_] := 
 Module[{posList},
  If[dirOfPlay == "Down", posList = {ToUpperCase[FromLetterNumber[boardRow - retrace]], boardCol + toTile};
   Flatten[Outer[List, posList[[1]], posList[[2]]], 1],
   posList = {ToUpperCase[FromLetterNumber[boardRow + toTile]], boardCol - retrace};
   Flatten[Outer[List, posList[[1]], posList[[2]]], 1]
   ]
  ]

(* 
   Takes the current Master Association, the word to be played, and the possible options for the overlap tile,
   and finds an association in the form:
   <| "Down" -> <| "OverlapTileOption" -> {pos1, pos2, ...} |>, "Right" -> <|...|> |>
   Then returns a list of valid 'overlap vectors' subject to board constraints.
*)
FindPossibleOverlapPositions[assoc_, word_, overlapTileOptions_List] :=
   Module[{overlapAssoc = <|"Down" -> <||>, "Right" -> <||>|>, overlapVectors, overlapSquaresAssoc},
      Map[ (* Map over overlap tiles. *)
         Function[{tile},
            Map[ (* Map over played bingos. *)
               Function[{i},
                  Module[{direction, boardRow, boardCol, bingo = assoc[[i]]["Bingo"],
                          toTile, retrace,
                          avoidCols, posListDown, filterPosListDown,
                          avoidRows, posListRight, filterPosListRight
                      },             
                      direction = assoc[[i]]["Direction"]; (* Find direction of played bingo. *)
                      {boardRow, boardCol} = assoc[[i]]["Position"]; (* Find position of played bingo. *)
                      boardRow = LetterNumber[boardRow];
                      toTile = StringPosition[assoc[[i]]["Bingo"], tile][[All, 1]] - 1; (* Find position of overlap tile within played bingo. *)    
                      retrace = StringPosition[word, tile][[All, 1]] - 1; (* Find position of overlap tile within new bingo. *) 

                      (* Play "Down" through this already-played word, as it is directed to the "Right". *)
                      If[direction == "Right",
                        avoidCols = Values[Select[assoc, #["Direction"] == "Down" &][[All, "Position"]]][[All, 2]]; (* Do not play "Down" through existing "Down" bingos. *)   
                        posListDown = FindStartingSquares[boardRow, boardCol, retrace, toTile, "Down"];
                        filterPosListDown = Select[posListDown, FreeQ[avoidCols, #[[2]]] && LetterNumber[#[[1]]] <= 8 && LetterNumber[#[[1]]] >= 1 &];

                        If[filterPosListDown =!= {},
                          If[! KeyExistsQ[overlapAssoc["Down"], tile], overlapAssoc["Down"][tile] = <||>];
                          If[! KeyExistsQ[overlapAssoc["Down"][tile], bingo], overlapAssoc["Down"][tile][bingo] = {}];
                        ];
                        If[filterPosListDown =!= {} && KeyExistsQ[overlapAssoc["Down"][tile], bingo], overlapAssoc["Down"][tile][bingo] = Join[overlapAssoc["Down"][tile][bingo], filterPosListDown]
                         ], (* Update overlapAssoc. *)
                        
                      (* Else, play "Right" through this already-played word, as it is directed "Down". *)
                        avoidRows = Values[Select[assoc, #["Direction"] == "Right" &][[All, "Position"]]][[All, 1]]; (* Do not play "Right" through existing "Right" bingos. *)   
                        posListRight = FindStartingSquares[boardRow, boardCol, retrace, toTile, "Right"];
                        filterPosListRight = Select[posListRight, FreeQ[avoidRows, #[[1]]] && #[[2]] > 0 && #[[2]] <= 8 &];

                        If[filterPosListRight =!= {},
                          If[! KeyExistsQ[overlapAssoc["Right"], tile], overlapAssoc["Right"][tile] = <||>];
                          If[! KeyExistsQ[overlapAssoc["Right"][tile], bingo], overlapAssoc["Right"][tile][bingo] = {}];
                        ];
                        If[filterPosListRight =!= {} && KeyExistsQ[overlapAssoc["Right"][tile], bingo], overlapAssoc["Right"][tile][bingo] = Join[overlapAssoc["Right"][tile][bingo], filterPosListRight]
                         ]
                      ]
                   ]
                ],
               Range[Length[assoc]]
             ]
          ],
         overlapTileOptions
       ];
      overlapAssoc = Select[overlapAssoc, # =!= <||> &]; (* E.g. after turn 1 there will be no "Right" overlaps. *)
      overlapVectors = Flatten[Table[{#, dir, tile, bingo} & /@ overlapAssoc[dir][tile][bingo], {dir, Keys[overlapAssoc]}, {tile, Keys[overlapAssoc[dir]]}, {bingo, Keys[overlapAssoc[dir, tile]]}], 3]; (* Obtain all combinations of position, direction, and tile. *)

      (* For each vector, where is it overlapping with 'forbidden' squares? *)
      overlapSquaresAssoc = Map[
        # -> Intersection[
          Battleship[word, #[[1]], #[[2]]], 
          UpdateForbiddenSquares[assoc, #[[4]]]
        ] &, 
          overlapVectors
      ];
      Keys[Select[overlapSquaresAssoc, Length[Values[#]] == 1 &]] (* Select overlap vectors that overlap exactly one square. *)
    ]

(* Identify squares used up by a 'word' placed at 'startPos'. *)
Battleship[word_, startPos_, direction_] :=
 Module[{row, col, length = StringLength[word]},
    {row, col} = startPos;
    If[direction === "Right",
      Table[{row, col + i}, {i, 0, length - 1}],
      Table[{ToUpperCase[FromLetterNumber[LetterNumber[row] + i]], col}, {i, 0, length - 1}]
  ]
 ]

(* Identify squares in the 'forbidden zones' around already played words. *)
ForbiddenSquares[word_, startPos_, direction_] :=
 Module[{row, col, length = StringLength[word], 
    mainPositions, 
    abovePositions, belowPositions, 
    leftPositions, rightPositions
   },
    {row, col} = startPos;
    If[direction === "Right",
      mainPositions = Table[{row, col + i}, {i, -1, length}];
      abovePositions = Table[{ToUpperCase[FromLetterNumber[LetterNumber[row] - 1]], col + i}, {i, 0, length - 1} ];
      belowPositions = Table[{ToUpperCase[FromLetterNumber[LetterNumber[row] + 1]], col + i}, {i, 0, length - 1} ];
      Join[mainPositions, abovePositions, belowPositions],
   
      mainPositions = Table[{ToUpperCase[FromLetterNumber[LetterNumber[row] + i]], col}, {i, -1, length}];
      leftPositions = Table[{ToUpperCase[FromLetterNumber[LetterNumber[row] + i]], col - 1}, {i, 0, length - 1}];
      rightPositions = Table[{ToUpperCase[FromLetterNumber[LetterNumber[row] + i]], col + 1}, {i, 0, length - 1}];
      Join[mainPositions, leftPositions, rightPositions]
   ]
  ]

(* Update 'forbidden squares taking into account the bingo to be overlapped. *)
UpdateForbiddenSquares[assoc_, bingo_] :=
  DeleteDuplicates[
   Flatten[
    Values[
     Map[
      With[{word = #["Bingo"], pos = #["Position"], 
         dir = #["Direction"]},
        
        If[word == bingo, Battleship[word, pos, dir], 
         ForbiddenSquares[word, pos, dir]]
        ] &,
      assoc
      ]
     ],
    1
    ]
   ]

(* Uses 'blankAssoc' to retrieve the blank(s) required for this play. *)
IdentifyBlanks[overlapVector_, blankAssoc_] := SelectFirst[blankAssoc, #["Overlap"] == overlapVector[[3]] &]["Blank"]

(* Formats word ready for it to be displayed with its blanks as "?" tiles. *)
FormatWordWithBlanks[word_, blanks_] := Module[{formatWord = word},
    Map[
       Module[
        {blankIndices = StringPosition[word, #][[All, 1]]},
      If[Length[blankIndices] > Count[blanks, #], blankIndices = {RandomChoice[blankIndices]}];
     formatWord = StringReplacePart[formatWord, ToLowerCase[#], {#, #} & /@ blankIndices]
         ] &,
       blanks
     ];
    Return[formatWord]
  ]

(* Determines the blanks allowed at this stage of the iteration. *)
BlanksAllowed[assoc_, usedCounts_] := 
 Module[{blanksUsed = usedCounts["?"], b},
 If[Length[assoc] < 12, b = 0,
   If[Length[assoc] == 13 && blanksUsed == 0, b = 2, b = 1]
 ];
  Return[b]
  ]

(* Algorithmically generates valid bingos and displays them on the Scrabble board to be chosen by the user. *)
RunScrabblegorithm[] :=
Module[
  {
  wordsByLength = GroupBy[RandomSample[Import["CSW21.txt", "List"]], StringLength],
  board = CreateInitialScrabbleBoard[], (* Create Graphic of Empty Scrabble Board. *)
  assoc = <||>
  },
  i = 1;
  While[Length[assoc] < 14,
    (* Turn 1. *)
    While[Length[assoc] == 0,
      remainingCounts = tiles[[All, "Quantity"]]; (* Whole tile bag remaining. *)
      usedCounts = AssociationThread[Keys[tiles], Table[0, 27]]; (* Initially zero tiles have been used. *)
      b = BlanksAllowed[assoc, usedCounts];
      Module[{next = False},
        pos = {"H", 2};
        word = wordsByLength[7][[i]];
        If[UpdateRemainingTileCount[remainingCounts, word, b] === tiles[[All, "Quantity"]], (* Starting 7-letter word must not require blanks. *)
          Print[word, ": Choose another Starter"];
          next = True
          ];
        Module[{boardTurn1},
          boardTurn1 =
            Row[{
              DynamicModule[
                {currentBoard = board, localEpilogState},
                localEpilogState = UpdateScrabbleBoard[word, pos, "Right", {}];
                Column[{
                  Show[currentBoard, ImageSize -> 300, Epilog -> localEpilogState],
                  Button[
                    "Select",
                    AppendTo[assoc, 1 -> <|"Bingo" -> word, "Position" -> pos, "Direction" -> "Right"|>];
                    remainingCounts = UpdateRemainingTileCount[remainingCounts, word, b];
                    usedCounts = UpdateUsedTileCount[usedCounts, word];
                    epilogState = localEpilogState;
                    next = True;(* Flag is set to True when selected *)
                    Print["Tiles Used: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]]];
                    Print["Tiles Left: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]]];
                    NotebookDelete[EvaluationCell[]];
                  ]
                }]
              ],
              Button["Skip", next = True; NotebookDelete[EvaluationCell[]]]
            }];
          If[next == False, Print[boardTurn1]];
          WaitUntil[next === True];
          next = False;
        ];
      ];
    i++;
    If[i > Length[wordsByLength[7]], Abort[]];
    If[Length[assoc] == 1, i = 1]
    ];
  
    (* Turns 2-12. *)
    While[1 <= Length[assoc] < 12,
      Module[{next = False}, (* Initialize flag. *)
        word = wordsByLength[8][[i]];
        overlapTileOptions = Intersection[Characters[word], Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]];
        If[overlapTileOptions === {}, Print[word, " Has No Overlaps!"]; next = True];
        b = BlanksAllowed[assoc, usedCounts];
        Map[ (* Check the remaining counts for each overlap option. *)
          If[
            UpdateRemainingTileCount[remainingCounts, StringJoin[DeleteElements[Characters[word], 1 -> {#}]], b] == remainingCounts,
            overlapTileOptions = DeleteCases[overlapTileOptions, #] (* Keep only options that can be formed with the available tiles. *)
            ] &, overlapTileOptions
          ];
        If[overlapTileOptions === {}, next = True];
        overlapVectors = FindPossibleOverlapPositions[assoc, word, overlapTileOptions];
        If[overlapVectors === {}, next = True];
        possibleEpilogs = AssociationMap[UpdateScrabbleBoard[word, #[[1]], #[[2]], epilogState] &, overlapVectors];
          
        Module[{boardOptions},
          boardOptions =
            Row[
              Join[
                Table[
                  DynamicModule[
                    {currentBoard = board, localEpilogState, dIdx = idx},
                    localEpilogState = possibleEpilogs[overlapVectors[[idx]]];
                    Column[{  
                      Show[currentBoard, ImageSize -> 300, Epilog -> Dynamic[localEpilogState]],
                      Button[
                        "Select",
                        AppendTo[assoc, Length[assoc] + 1 -> <|"Bingo" -> word, "Position" -> overlapVectors[[dIdx]][[1]], "Direction" -> overlapVectors[[dIdx]][[2]], "Overlap" -> overlapVectors[[dIdx]][[3]]|>];
                        remainingCounts = UpdateRemainingTileCount[remainingCounts, StringJoin[DeleteElements[Characters[word], 1 -> {overlapVectors[[dIdx]][[3]]}]], b];
                        usedCounts = UpdateUsedTileCount[usedCounts, StringJoin[DeleteElements[Characters[word], 1 -> {overlapVectors[[dIdx]][[3]]}]]];
                        epilogState = localEpilogState;
                        next = True; (* Flag is set to True when selected *)
                        Print["Tiles Used: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]]];
                        Print["Tiles Left: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]]];
                        Print["Bingos Checked: ", i];
                        NotebookDelete[EvaluationCell[]]
                    ]
                    }]
                  ],
                {idx, Length[overlapVectors]}
                ],
              {If[overlapTileOptions =!= {}, 
                Button["Skip", next = True; NotebookDelete[EvaluationCell[]]], Nothing
                ]}
              ]
            ];
          If[next == False, Print[boardOptions]];
          WaitUntil[next === True];
          next = False;
        ];
      ];
      i++; 
      If[i == 10000, Print["25% Bingos Checked"]];
      If[i == 20000, Print["50% Bingos Checked"]];
      If[i == 30000, Print["75% Bingos Checked"]];
      If[i > Length[wordsByLength[8]], Print["Sorry! Try Again!"]; Abort[]];
      If[Length[assoc] == 12, i = 1]
    ];
  
  (* Turns 13 and 14. *)
    Module[{next = False}, (* Initialize flag. *)
      word = wordsByLength[8][[i]];
      overlapTileOptions = Intersection[Characters[word], Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]]; 
      If[overlapTileOptions === {}, Print[word, " Has No Overlaps!"]; next = True];
      b = BlanksAllowed[assoc, usedCounts];
      blankAssoc = {};
        (* Identify blanks and update blankAssoc. *)
        Map[
          Module[{newRemainingCounts, negativeKeys},
            newRemainingCounts = UpdateRemainingTileCount[remainingCounts, StringJoin[DeleteElements[Characters[word], 1 -> {#}]], b];
            If[newRemainingCounts == remainingCounts,             
              overlapTileOptions = DeleteCases[overlapTileOptions, #],
              negativeKeys = Flatten[Table[#, {-newRemainingCounts[#]}] & /@ Select[Keys[newRemainingCounts], newRemainingCounts[#] < 0 &]];
              If[negativeKeys =!= {},
                AppendTo[blankAssoc, <|"Overlap" -> #, "Blank" -> negativeKeys|>],
                AppendTo[blankAssoc, <|"Overlap" -> #, "Blank" -> Null|>]
              ]
            ]
          ] &,
          overlapTileOptions
        ];
      If[overlapTileOptions === {}, next = True];
      overlapVectors = FindPossibleOverlapPositions[assoc, word, overlapTileOptions];
      If[overlapVectors === {}, next = True,
        Echo@word;
        {overlapVectors, possibleEpilogs} =
          Module[
            {vectors, epilogs,
            blankVectors, wordOptions, blankEpilogs},
            vectors = Select[overlapVectors, IdentifyBlanks[#, blankAssoc] == Null &];
            epilogs = AssociationMap[UpdateScrabbleBoard[word, #[[1]], #[[2]], epilogState] &, vectors];
            blankVectors = Select[overlapVectors, IdentifyBlanks[#, blankAssoc] =!= Null &];
            blankVectors = Map[Append[#, IdentifyBlanks[#, blankAssoc]] &, blankVectors];
            wordOptions = AssociationMap[FormatWordWithBlanks[word, IdentifyBlanks[#, blankAssoc]] &, blankVectors];
            blankEpilogs = AssociationMap[UpdateScrabbleBoard[StringReplace[wordOptions[#], c_ /; LowerCaseQ[c] :> "?"], #[[1]], #[[2]], epilogState] &, blankVectors];
            {Join[vectors, blankVectors], Join[epilogs, blankEpilogs]}
          ];
      ];
      Module[{boardOptions},
        boardOptions =
          Row[
            Join[
              Table[
                DynamicModule[
                  {currentBoard = board, localEpilogState, dIdx = idx},
                  localEpilogState = possibleEpilogs[overlapVectors[[idx]]];
                  Column[{ 
                    Show[currentBoard, ImageSize -> 300, Epilog -> Dynamic[localEpilogState]],
                    Button[
                      "Select",
                      i = 1;
                      AppendTo[assoc, Length[assoc] + 1 -> <|"Bingo" -> word, "Position" -> overlapVectors[[dIdx]][[1]], "Direction" -> overlapVectors[[dIdx]][[2]], "Overlap" -> overlapVectors[[dIdx]][[3]]|>];
                      If[Length[overlapVectors[[dIdx]]] == 5,
                        usedCounts = UpdateUsedTileCount[usedCounts, StringJoin[DeleteElements[Characters[word], 1 -> Flatten[{overlapVectors[[dIdx]][[3]], overlapVectors[[dIdx]][[5]]}]]]];
                        remainingCounts = UpdateRemainingTileCount[remainingCounts, StringJoin[DeleteElements[Characters[word], 1 -> {overlapVectors[[dIdx]][[3]]}]], Length[overlapVectors[[dIdx]][[5]]]];
                        Map[remainingCounts[#]++ &, overlapVectors[[dIdx]][[5]]];
                        usedCounts["?"] = usedCounts["?"] + Length[overlapVectors[[dIdx]][[5]]],
                        usedCounts = UpdateUsedTileCount[usedCounts, StringJoin[DeleteElements[Characters[word], 1 -> {overlapVectors[[dIdx]][[3]]}]]];
                        remainingCounts = UpdateRemainingTileCount[remainingCounts, StringJoin[DeleteElements[Characters[word], 1 -> {overlapVectors[[dIdx]][[3]]}]], 0]
                      ];
                      epilogState = localEpilogState;
                      next = True;(* Flag is set to True when selected *)
                      Print["Tiles Used: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, usedCounts]]]];
                      Print["Tiles Left: ", Length[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]], " ", StringJoin[Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]]];
                      NotebookDelete[EvaluationCell[]]
                    ]
                  }]
                ],
              {idx, Length[overlapVectors]}
              ],
              {If[overlapTileOptions =!= {}, 
                Button["Skip", next = True; NotebookDelete[EvaluationCell[]]], Nothing
              ]}
            ]
          ];
        If[next == False, Print[boardOptions]];
        WaitUntil[next === True];
        next = False;
      ];
    ];
    i++; 
    If[i == 10000, Print["25% Bingos Checked"]];
    If[i == 20000, Print["50% Bingos Checked"]];
    If[i == 30000, Print["75% Bingos Checked"]];
    If[i > Length[wordsByLength[8]],  Print["Sorry! Try Again!"]; Abort[]];
  ]
]

GameToEpilog[game_] := 
Fold[
  UpdateScrabbleBoard[
    game[#2, "Bingo"],
    game[#2, "Position"],
    game[#2, "Direction"],
    #1
  ] &,
  {},
  Range[Length[game]]
]

PerfectScrabbleGameQ[game_] :=
 Module[{
   tileBag = KeyDrop[tiles[[All, "Quantity"]], "?"],
   bingoChars = Flatten[Characters /@ Values[game[[All, "Bingo"]]]],
   overlapCounts = Counts[Values[game[[All, "Overlap"]]][[2 ;;]]],
   playedTilesCounts = AssociationThread[CharacterRange["A", "Z"], ConstantArray[0, Length[CharacterRange["A", "Z"]]]],
   diff, blanks, leftovers
   },
  playedTilesCounts = KeySort[Merge[{playedTilesCounts, Counts[DeleteElements[bingoChars, Values[overlapCounts] -> Keys[overlapCounts]]]}, Total]];
  diff = tileBag - playedTilesCounts;
  blanks = Flatten[Table[#, {-diff[#]}] & /@ Keys[Select[diff, # < 0 &]]];
  leftovers = Keys[Select[diff, # > 0 &]];
  Return[{
    If[Total[diff] == 0, True, False], 
    Grid[{{"Blanks:", blanks}, {" Leftovers:", leftovers}}]
    }]
  ]

End[]

EndPackage[]