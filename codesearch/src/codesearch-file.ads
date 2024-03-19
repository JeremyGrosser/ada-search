with Codesearch.Strings; use Codesearch.Strings;

package Codesearch.File is

   type Relative_Path is new Unicode;
   type Absolute_Path is new Unicode;

   function Join
      (Left, Right : Relative_Path)
      return Relative_Path;

   function Normalize
      (Path : Relative_Path)
      return Absolute_Path;

   function Exists
      (Filename : String)
      return Boolean;

   function Length
      (Filename : String)
      return Natural;

   procedure Read
      (Filename : String;
       Data : out String);

   function Read_Unicode
      (Filename : UTF8)
      return Unicode;

end Codesearch.File;
