package Codesearch.File is

   function Exists
      (Filename : String)
      return Boolean;

   function Length
      (Filename : String)
      return Natural;

   procedure Read
      (Filename : String;
       Data : out String);

end Codesearch.File;
