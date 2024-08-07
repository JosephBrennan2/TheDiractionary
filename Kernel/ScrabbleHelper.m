BeginPackage["TheDiractionary`ScrabbleHelper`"];
formatWord;
ScrabbleHelper;
ScrabbleBingo;

Begin["`Private`"]


formatWord[word_, letter_] := Module[{positions},
    positions = StringPosition[word, letter];
    If[Length[positions] > 0,
       StringReplacePart[word, "(" <> letter <> ")", positions[[1]]],
       word
     ]
  ]

ScrabbleHelper[rack_ , dict_] := Module[{
   tiles = Sort[Characters[rack]],
   wordsByLength = GroupBy[dict, StringLength],
   usingBoard = 
    Table[Sort[Characters[rack <> letter]], {letter, 
      CharacterRange["A", "Z"]}] ,
   sevens,
   eights,
   formattedEights},
  sevens = 
   Select[wordsByLength[7], SameQ[Sort[Characters[#]], tiles] &];
  
  eights = Flatten[Table[
        Map[
          {#, letter} &,
          
      Select[wordsByLength[8], SameQ[Sort[Characters[#]], tile] &]
         ],
        {letter, CharacterRange["A", "Z"]},
        {tile, {Sort[Characters[rack <> letter]]}}
       ], 2];
  	formattedEights = Map[formatWord @@ # &, eights];
  TableForm[{Prepend[sevens, {"7 Letter Bingos"}], 
    Prepend[formattedEights, {"8 Letter Bingos"}]}
   ]
  ]

ScrabbleBingo[rack_, dict_] := 
 Module[{sevens, sortedCharSevens, bingoPositions, letters, possibleRacks, blankPos}, 
  sevens = Select[dict, StringLength[#] == 7 &];
  sortedCharSevens = Map[Sort, Characters[ToUpperCase[sevens]]];
  letters = CharacterRange["A", "Z"];
  blankPos = StringPosition[rack, "?"];
  Switch[Length[blankPos],
           0,(* If there are no blanks. *)
           possibleRacks = {rack},
           1,(* If there is one blank. *)
           possibleRacks = StringReplacePart[rack, #, blankPos[[1]]] & /@ letters,
           2,(* If there are two blanks. *)          
           possibleRacks = Flatten[Outer[StringReplacePart[rack, {#1, #2}, blankPos] &, letters, letters, 1]]
       ];
      bingoPositions = Flatten[Position[sortedCharSevens, Sort[Characters[ToUpperCase[#]]]] & /@ possibleRacks];
      If[Length[bingoPositions] > 0, 
            DeleteDuplicates[ToUpperCase[sevens[[#]]] & /@ bingoPositions],
            Return["No Scrabble Bingos"]
       ]
  ]

End[]

EndPackage[]