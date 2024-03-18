with VSS.Strings; use VSS.Strings;

package Codesearch.Database is

   type Search_Result is record
      Crate    : Virtual_String;
      Filename : Virtual_String;
      Path     : Virtual_String;
      Rank     : Integer;
   end record;

   subtype Search_Result_Index is Positive range 1 .. 250;
   type Search_Results is array (Search_Result_Index range <>) of Search_Result;

   procedure Initialize;

   procedure Search
      (Query   : String;
       Results : out Search_Results;
       Last    : out Natural);

   procedure Close;

end Codesearch.Database;
