SetDirectory[ParentDirectory @ DirectoryName @ $InputFileName];

Get[#]& /@ {
    "TheDiractionary`ScrabbleScore`",
    "TheDiractionary`ScrabbleHelper`",
    "TheDiractionary`Version1`"
  (* .m File Names. *)
};