SetDirectory[ParentDirectory @ DirectoryName @ $InputFileName];

Get[#]& /@ {
    "TheDiractionary`ScrabbleScore`",
    "TheDiractionary`ScrabbleHelper`",
    "TheDiractionary`Scrabble-gorithm`",
    "TheDiractionary`ScrabbleBoard`"
  (* .m File Names. *)
};