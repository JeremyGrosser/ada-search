with Codesearch.Strings; use Codesearch.Strings;
private with Sqlite;

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

   type Session is private;

   procedure Create;

   function Open
      (Read_Only : Boolean := True)
      return Session;

   procedure Search
      (This    : Session;
       Query   : Unicode;
       Results : out Search_Results;
       Last    : out Natural);

   procedure Add
      (This : in out Session;
       Crate, Path, Filename, Text : String);

   function Get_Text
      (This : Session;
       Path : Unicode)
      return String;

   function Get_Highlight
      (This : Session;
       Path : Unicode)
       return String;

   function Exists
      (This : Session;
       Path : Unicode)
       return Boolean;

   procedure Close
      (This : in out Session);

private

   type Query_Type is
      (Create_FTS,
       Create_Path_Hash,
       Create_Path_Hash_Index,
       Create_Content,
       Create_Content_Index,

       Insert_FTS,
       Insert_Path_Hash,
       Insert_Content,
       Select_Last_Row_Id,

       Select_FTS,
       Select_Path_Hash,
       Select_Content);

   subtype Create_Query is Query_Type range Create_FTS .. Create_Content_Index;
   subtype Insert_Query is Query_Type range Insert_FTS .. Select_Last_Row_Id;
   subtype Select_Query is Query_Type range Select_FTS .. Select_Content;

   type Statements is array (Query_Type) of Sqlite.Statement;

   type Session is record
      DB   : Sqlite.Connection;
      Stmt : Statements;
      Insert_Row_Id : Natural := 0;
   end record;

end Codesearch.Database;
