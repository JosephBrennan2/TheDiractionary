BeginPackage["TheDiractionary`Version1`", {"TheDiractionary`ScrabbleScore`"}];
updateUsedCount;
reduceRemainingCount;
RunVersion1Scrabblegorithm;

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
  Module[{j = 1, dict, wordsByLength, remainingCounts, usedCounts,
       bingos, blanks, b, word, newRemainingCounts, negativeKeys, i},
     Do[
       Print[StringJoin["Attempt ", ToString[j]]];
       j++;
       (*Initialize variables*)
       dict = RandomSample[Import["CSW21.txt", "List"]];
       wordsByLength = GroupBy[dict, StringLength];
       wordsByLength[7];
       remainingCounts = tiles[[All, "Quantity"]];
       usedCounts = AssociationThread[Keys[tiles], Table[0, 27]];
       bingos = {};
       blanks = {};
       i = 1;
       (*Main loop.*)
       While[
     i <= Length[wordsByLength[7]],
         word = wordsByLength[7][[i]];
         If[Length[bingos] < 12, b = 0, b = 1];
         
     newRemainingCounts = 
      reduceRemainingCount[remainingCounts, word, b];
         If[newRemainingCounts === remainingCounts, i++,
           
      negativeKeys = 
       Select[Keys[newRemainingCounts], newRemainingCounts[#] < 0 &];
           If[negativeKeys =!= {},
             AppendTo[blanks, negativeKeys[[1]]];
             newRemainingCounts[negativeKeys[[1]]] = 0;
             ];
           AppendTo[bingos, word];
           usedCounts = updateUsedCount[usedCounts, word];
           remainingCounts = newRemainingCounts;
           If[
       Length[bingos] == 14,
       Print["Success"];
       CloudPut[
        Append[CloudGet["V.1-WordMaster"],
         <|"Bingos" -> bingos, "Blanks" -> blanks, 
          "Tiles Remaining" -> 
           Flatten[KeyValueMap[Table[#1, #2] &, remainingCounts]]|>], 
        "V.1-WordMaster"]
       ];
           If[Length[bingos] == 12, i = 1, i++]
           ];
         ],
    {iterations}]
     ];

End[]

EndPackage[]