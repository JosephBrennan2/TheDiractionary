BeginPackage["TheDiractionary`ScrabbleBoard`"];
createScrabbleBoard;

Begin["`Private`"]

createScrabbleBoard[] := Module[{board , colors},
  
  board = {
            {"TW", "SL", "SL", "DL", "SL", "SL", "SL", "TW", "SL", "SL", "SL", "DL", "SL", "SL", "TW"}, 
            {"SL", "DW", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "DW", "SL"}, 
            {"SL", "SL", "DW", "SL", "SL", "SL", "DL", "SL", "DL", "SL", "SL", "SL", "DW", "SL", "SL"},
            {"DL", "SL", "SL", "DW", "SL", "SL", "SL", "DL", "SL", "SL", "SL", "DW", "SL", "SL", "DL"},
            {"SL", "SL", "SL", "SL", "DW", "SL", "SL", "SL", "SL", "SL", "DW", "SL", "SL", "SL", "SL"}, 
            {"SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "TL", "SL"}, 
            {"SL", "SL", "DL", "SL", "SL", "SL", "DL", "SL", "DL", "SL", "SL", "SL", "DL", "SL", "SL"},
            {"TW", "SL", "SL", "DL", "SL", "SL", "SL", "DW", "SL", "SL", "SL", "DL", "SL", "SL", "TW"}, 
            {"SL", "SL", "DL", "SL", "SL", "SL", "DL", "SL", "DL", "SL", "SL", "SL", "DL", "SL", "SL"},
            {"SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "TL", "SL"}, 
            {"SL", "SL", "SL", "SL", "DW", "SL", "SL", "SL", "SL", "SL", "DW", "SL", "SL", "SL", "SL"}, 
            {"DL", "SL", "SL", "DW", "SL", "SL", "SL", "DL", "SL", "SL", "SL", "DW", "SL", "SL", "DL"}, 
            {"SL", "SL", "DW", "SL", "SL", "SL", "DL", "SL", "DL", "SL", "SL", "SL", "DW", "SL", "SL"}, 
            {"SL", "DW", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "TL", "SL", "SL", "SL", "DW", "SL"}, 
            {"TW", "SL", "SL", "DL", "SL", "SL", "SL", "TW", "SL", "SL", "SL", "DL", "SL", "SL", "TW"}
          };
  colors = {
    		"TW" -> Red,
    		"SL" -> Darker[Green],
    		"DL" -> Cyan,
    		"TL" -> Blue,
    		"DW" -> Orange
    	    };
  ArrayPlot[board, ColorRules -> colors, Mesh -> True, MeshStyle -> Black]
  ]


End[]

EndPackage[]