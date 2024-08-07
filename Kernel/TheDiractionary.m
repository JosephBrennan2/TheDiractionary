SetDirectory[ParentDirectory @ DirectoryName @ $InputFileName];

Get[#]& /@ {
    "TheDiractionary`ScrabbleScore`"
  (* .m File Names. *)
};