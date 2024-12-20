with Hex_Format_8;
with SHA3;

package body Codesearch.Database is

   Database_Path : constant String := "index.db";

   function SQL
      (Q : Query_Type)
      return String
   is
   begin
      case Q is
         when Create_FTS =>
            return "CREATE VIRTUAL TABLE f USING fts5(crate, path, filename, hash, text)";
         when Insert_FTS =>
            return "INSERT INTO f(rowid, crate, path, filename, hash, text) VALUES (?, ?, ?, ?, ?, ?)";
         when Select_FTS =>
            return "SELECT rowid, crate, path, filename, hash, rank FROM f " &
            "WHERE text MATCH ? ORDER BY rank LIMIT 250";

         when Create_Path_Hash =>
            return "CREATE TABLE path_hash (path TEXT, hash TEXT)";
         when Insert_Path_Hash =>
            return "INSERT OR IGNORE INTO path_hash (path, hash) VALUES (?, ?)";
         when Select_Path_Hash =>
            return "SELECT hash FROM path_hash WHERE path=? LIMIT 1";

         when Create_Content =>
            return "CREATE TABLE content (hash TEXT, data BLOB)";
         when Insert_Content =>
            return "INSERT INTO content (hash, data) VALUES (?, ?)";
         when Select_Content =>
            return "SELECT data FROM content WHERE hash=? LIMIT 1";
      end case;
   end SQL;

   subtype Write_Query is Query_Type range Create_Query'First .. Insert_Query'Last;

   procedure Execute
      (This : Session;
       Q    : Write_Query)
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      Status := Sqlite.Step (This.DB, This.Stmt (Q));
      if Status /= Sqlite.SQLITE_DONE then
         raise Program_Error with "Unable to execute " & Q'Image & ": " & Status'Image;
      end if;
   end Execute;

   procedure Create is
      S : Session;
   begin
      Sqlite.Initialize;
      S.DB := Sqlite.Open (Database_Path,
         (CREATE  => True,
          WAL     => True,
          others  => False));
      if not Sqlite.Is_Open (S.DB) then
         raise Program_Error with "Unable to open index.db";
      end if;

      for Query in Create_Query'Range loop
         S.Stmt (Query) := Sqlite.Prepare (S.DB, SQL (Query));
         Execute (S, Query);
      end loop;

      Close (S);
   end Create;

   function Open
      (Read_Only : Boolean := True)
      return Session
   is
      This : Session;
   begin
      Sqlite.Initialize;
      This.DB := Sqlite.Open (Database_Path,
         (READONLY => Read_Only,
          NOMUTEX  => True,
          WAL      => True,
          others   => False));
      if not Sqlite.Is_Open (This.DB) then
         raise Program_Error with "Unable to open index.db";
      end if;

      for Query in Select_Query'Range loop
         This.Stmt (Query) := Sqlite.Prepare (This.DB, SQL (Query));
      end loop;

      if not Read_Only then
         for Query in Insert_Query'Range loop
            This.Stmt (Query) := Sqlite.Prepare (This.DB, SQL (Query));
         end loop;
      end if;

      return This;
   end Open;

   procedure Search
      (This    : Session;
       Query   : Unicode;
       Results : out Search_Results;
       Last    : out Natural)
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
      Stmt : Sqlite.Statement renames This.Stmt (Select_FTS);

      NUL : constant Wide_Wide_Character := Wide_Wide_Character'Val (0);
      Q : Unbounded_Unicode;
   begin
      Last := 0;

      if not Sqlite.Is_Open (This.DB) then
         raise Program_Error with "Database not open";
      end if;

      Append (Q, '"');
      for Ch of Query loop
         case Ch is
            when NUL | '"' =>
               null;
            when others =>
               Append (Q, Ch);
         end case;
      end loop;
      Append (Q, '"');

      --  stripped everything except quotes
      if Length (Q) = 2 then
         return;
      end if;

      Sqlite.Reset (This.DB, Stmt);
      Sqlite.Bind_Text (This.DB, Stmt, 1, String (Encode (Q)));

      for I in Results'Range loop
         Status := Sqlite.Step (This.DB, Stmt);
         exit when Status /= Sqlite.SQLITE_ROW;
         Results (I) :=
            (Id         => Natural (Sqlite.To_Integer (Stmt, 0)),
             Crate      => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Stmt, 1)))),
             Path       => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Stmt, 2)))),
             Filename   => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Stmt, 3)))),
             Hash       => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Stmt, 4)))),
             Rank       => Sqlite.To_Integer (Stmt, 5));
         Last := I;
      end loop;

      case Status is
         when Sqlite.SQLITE_ROW | Sqlite.SQLITE_DONE =>
            null;
         when others =>
            raise Program_Error with "Query returned error: " & Status'Image;
      end case;
   end Search;

   function Content_Hash
      (Text : String)
      return String
   is
      function To_String
         (Digest : SHA3.SHA3_256.Digest_Type)
         return String
      is
         use Hex_Format_8;
         S : String (1 .. Digest'Length * 2);
         I : Positive := S'First;
      begin
         for D of Digest loop
            S (I .. I + 1) := Hex (D);
            I := I + 2;
         end loop;
         return S;
      end To_String;

      use SHA3.SHA3_256;
      Input  : Byte_Array (1 .. Text'Length)
         with Import, Address => Text'Address;
      Digest : Digest_Type;
      Ctx    : Context;
   begin
      Init (Ctx);
      Update (Ctx, Input);
      Final (Ctx, Digest);
      return To_String (Digest);
   end Content_Hash;

   function Get_Hash
      (This : Session;
       Path : Unicode)
      return String
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
      Stmt : Sqlite.Statement renames This.Stmt (Select_Path_Hash);
   begin
      Sqlite.Reset (This.DB, Stmt);
      Sqlite.Bind_Text (This.DB, Stmt, 1, String (Encode (Path)));
      Status := Sqlite.Step (This.DB, Stmt);
      if Status = Sqlite.SQLITE_ROW then
         return Sqlite.To_String (Stmt, 0);
      elsif Status = Sqlite.SQLITE_DONE then
         return "";
      else
         raise Program_Error with "Select hash returned error: " & Status'Image;
      end if;
   end Get_Hash;

   procedure Add_Hash
      (This : Session;
       Path : Unicode;
       Hash : String)
   is
      Stmt : Sqlite.Statement renames This.Stmt (Insert_Path_Hash);
   begin
      Sqlite.Reset (This.DB, Stmt);
      Sqlite.Bind_Text (This.DB, Stmt, 1, String (Encode (Path)));
      Sqlite.Bind_Text (This.DB, Stmt, 2, Hash);
      Execute (This, Insert_Path_Hash);
   end Add_Hash;

   function Content_Exists
      (This : Session;
       Hash : String)
      return Boolean
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
      Stmt : Sqlite.Statement renames This.Stmt (Select_Content);
   begin
      Sqlite.Reset (This.DB, Stmt);
      Sqlite.Bind_Text (This.DB, Stmt, 1, Hash);
      Status := Sqlite.Step (This.DB, Stmt);
      return Status = Sqlite.SQLITE_ROW;
   end Content_Exists;

   procedure Add_Content
      (This : Session;
       Hash : String;
       Data : String)
   is
      Stmt : Sqlite.Statement renames This.Stmt (Insert_Content);
   begin
      if not Content_Exists (This, Hash) then
         Sqlite.Reset (This.DB, Stmt);
         Sqlite.Bind_Text (This.DB, Stmt, 1, Hash);
         Sqlite.Bind_Text (This.DB, Stmt, 2, Data);
         Execute (This, Insert_Content);
      end if;
   end Add_Content;

   function Get_Content_By_Hash
      (This : Session;
       Hash : String)
      return String
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
      Stmt : Sqlite.Statement renames This.Stmt (Select_Content);
   begin
      Sqlite.Reset (This.DB, Stmt);
      Sqlite.Bind_Text (This.DB, Stmt, 1, Hash);
      Status := Sqlite.Step (This.DB, Stmt);
      if Status = Sqlite.SQLITE_ROW then
         return Sqlite.To_String (Stmt, 0);
      else
         return "";
      end if;
   end Get_Content_By_Hash;

   function Get_Text
      (This : Session;
       Path : Unicode)
      return String
   is (Get_Content_By_Hash (This, Get_Hash (This, Path)));

   procedure Add_FTS
      (This : in out Session;
       Crate, Path, Filename, Hash, Text : String)
   is
      Stmt : Sqlite.Statement renames This.Stmt (Insert_FTS);
   begin
      Sqlite.Reset (This.DB, Stmt);
      Sqlite.Bind_Text (This.DB, Stmt, 1, This.Insert_Row_Id'Image);
      Sqlite.Bind_Text (This.DB, Stmt, 2, Crate);
      Sqlite.Bind_Text (This.DB, Stmt, 3, Path);
      Sqlite.Bind_Text (This.DB, Stmt, 4, Filename);
      Sqlite.Bind_Text (This.DB, Stmt, 5, Hash);
      Sqlite.Bind_Text (This.DB, Stmt, 6, Text);
      Execute (This, Insert_FTS);
      This.Insert_Row_Id := This.Insert_Row_Id + 1;
   end Add_FTS;

   procedure Add
      (This : in out Session;
       Crate, Path, Filename, Text : String)
   is
      Hash : constant String := Content_Hash (Text);
   begin
      Add_Content (This, Hash, Text);
      Add_Hash (This, Decode (UTF8 (Path)), Hash);
      Add_FTS (This, Crate, Path, Filename, Hash, Text);
   end Add;

   procedure Close
      (This : in out Session)
   is
      use type Sqlite.Statement;
   begin
      for Query in This.Stmt'Range loop
         if This.Stmt (Query) /= null then
            Sqlite.Finalize (This.DB, This.Stmt (Query));
         end if;
      end loop;

      Sqlite.Close (This.DB);
   end Close;

end Codesearch.Database;
