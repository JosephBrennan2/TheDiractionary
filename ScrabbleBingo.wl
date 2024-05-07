dictionary = Union[WordData[], DictionaryLookup[], WordList[]];
ScrabbleBingo[rack_] := Module[
      {sevens, sortedCharSevens, bingoPositions, letters, 
   possibleRacks, blankPos},
      sevens = Select[dictionary, StringLength[#] == 7 &];
      sortedCharSevens = Map[Sort, Characters[ToUpperCase[sevens]]];
      letters = CharacterRange["A", "Z"];
  	
     blankPos = StringPosition[rack, "?"];
  	
      Switch[Length[blankPos],
           0,(* If there are no blanks. *)
           possibleRacks = {rack},
           1,(* If there is one blank. *)
           
   possibleRacks = 
    StringReplacePart[rack, #, blankPos[[1]]] & /@ letters,
           2,(* If there are two blanks. *)
           
   possibleRacks = 
    Flatten[Outer[StringReplacePart[rack, {#1, #2}, blankPos] &, 
      letters, letters, 1]]
       ];
  		
      bingoPositions = 
   Flatten[Position[sortedCharSevens, 
       Sort[Characters[ToUpperCase[#]]]] & /@ possibleRacks];
  	
      If[Length[bingoPositions] > 0,
    DeleteDuplicates[ToUpperCase[sevens[[#]]] & /@ bingoPositions],
           Print["No Scrabble Bingos"]
       ]
  ]