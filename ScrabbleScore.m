BeginPackage["TheDiractionary`ScrabbleScore`"];
tiles;
ScrabbleScore;

Begin["`Private`"]
tiles = <|
   "A" -> <|"Quantity" -> 9, "Points" -> 1|>,
   "B" -> <|"Quantity" -> 2, "Points" -> 3|>,
   "C" -> <|"Quantity" -> 2, "Points" -> 3|>,
   "D" -> <|"Quantity" -> 4, "Points" -> 2|>,
   "E" -> <|"Quantity" -> 12, "Points" -> 1|>,
   "F" -> <|"Quantity" -> 2, "Points" -> 4|>,
   "G" -> <|"Quantity" -> 3, "Points" -> 2|>,
   "H" -> <|"Quantity" -> 2, "Points" -> 4|>,
   "I" -> <|"Quantity" -> 9, "Points" -> 1|>,
   "J" -> <|"Quantity" -> 1, "Points" -> 8|>,
   "K" -> <|"Quantity" -> 1, "Points" -> 5|>,
   "L" -> <|"Quantity" -> 4, "Points" -> 1|>,
   "M" -> <|"Quantity" -> 2, "Points" -> 3|>,
   "N" -> <|"Quantity" -> 6, "Points" -> 1|>,
   "O" -> <|"Quantity" -> 8, "Points" -> 1|>,
   "P" -> <|"Quantity" -> 2, "Points" -> 3|>,
   "Q" -> <|"Quantity" -> 1, "Points" -> 10|>,
   "R" -> <|"Quantity" -> 6, "Points" -> 1|>,
   "S" -> <|"Quantity" -> 4, "Points" -> 1|>,
   "T" -> <|"Quantity" -> 6, "Points" -> 1|>,
   "U" -> <|"Quantity" -> 4, "Points" -> 1|>,
   "V" -> <|"Quantity" -> 2, "Points" -> 4|>,
   "W" -> <|"Quantity" -> 2, "Points" -> 4|>,
   "X" -> <|"Quantity" -> 1, "Points" -> 8|>,
   "Y" -> <|"Quantity" -> 2, "Points" -> 4|>,
   "Z" -> <|"Quantity" -> 1, "Points" -> 10|>,
   "?" -> <|"Quantity" -> 2, "Points" -> 0|>
   |>;

ScrabbleScore[string_String] :=
   Block[{uString, pointsLookup, counts, score, diffs, blanks, 
   blankPoints, blankCount},
      uString = ToUpperCase[string];
      If[Length[Characters[uString]] > 15,
         
   Return["Error: The input string must be 15 characters or less."]
       ];
      If[
   Not[AllTrue[Characters[uString], # === "?" || LetterQ[#] &]],
         Return["Error: The input string contains invalid characters."]
       ];
      blankCount = Count[Characters[uString], "?"];
      If[blankCount > 2,
         Return["Error: No more than 2 blanks are allowed."]
       ];
      pointsLookup = 
   AssociationMap[Lookup[tiles, #, <|"Points" -> 0|>]["Points"] &, 
    Characters[uString]];
      counts = Counts[Characters[uString]];
      score = Total[Lookup[pointsLookup, Characters[uString]]];
      diffs = 
   AssociationMap[(Max[counts[#] - tiles[#][["Quantity"]], 0]) &, 
    Keys[counts]];
      blanks = DeleteCases[diffs, _ -> 0];
      If[Total[Values[blanks]] > 2,
         Return["Error: Impossible to construct."],
         
   blankPoints = 
    Total[Lookup[pointsLookup, Keys[blanks]]*Values[blanks]];
         score - blankPoints
       ]
    ]
End[]

EndPackage[]