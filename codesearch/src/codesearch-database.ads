with Codesearch.Strings; use Codesearch.Strings;

package Codesearch.Database is

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

   function Get_Hash
      (Path : Unicode)
      return String;

   procedure Add_Hash
      (Path : Unicode;
       Hash : String);

   procedure Add
      (Crate, Path, Filename, Hash, Text : String);

   procedure Close;

end Codesearch.Database;
