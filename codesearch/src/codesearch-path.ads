package Codesearch.Path is

   --  Ignore the previous parts if a part is absolute
   --  Insert a '/' unless the first part is empty or already ends in '/'
   function Join
      (Left, Right : String)
      return String;

   function Basename
      (P : String)
      return String;

   function Dirname
      (P : String)
      return String;

end Codesearch.Path;
