with Codesearch.Strings; use Codesearch.Strings;

package Codesearch.File is

   type Path is new Unicode;

   Path_Error : exception;

   function To_String
      (Name : Path)
      return String;

   function Join
      (Left, Right : Path)
      return Path;

   function Normalize
      (Name : Path)
      return Path;

   function Basename
      (Name : Path)
      return Path;

   function Remove_Extension
      (Name : Path)
      return Path;

   function Exists
      (Name : Path)
      return Boolean;

   function Length
      (Name : Path)
      return Natural;

   function Read_Unicode
      (Name : Path)
      return Unicode;

end Codesearch.File;
