with Codesearch.Strings; use Codesearch.Strings;

package Codesearch.Database
   with Elaborate_Body
is

   type Search_Result is record
      Id       : Natural;
      Crate    : Unbounded_Unicode;
      Path     : Unbounded_Unicode;
      Filename : Unbounded_Unicode;
      Hash     : Unbounded_Unicode;
      Rank     : Integer;
   end record;

   subtype Search_Result_Index is Positive range 1 .. 250;
   type Search_Results is array (Search_Result_Index range <>) of Search_Result;

   procedure Create;

   procedure Open
      (Read_Only : Boolean := True);

   procedure Search
      (Query   : Unicode;
       Results : out Search_Results;
       Last    : out Natural);

   procedure Add
      (Crate, Path, Filename, Text : String);

   function Get_Text
      (Path : Unicode)
      return String;

   procedure Close;

end Codesearch.Database;
