with Sqlite;
with Hex_Format_8;
with SHA3;

package body Codesearch.Database is

   Database_Path : constant String := "index.db";

   Insert_FTS_Query : constant String :=
      "INSERT INTO f(rowid, crate, path, filename, hash, text) VALUES (?, ?, ?, ?, ?, ?)";
   Insert_Hash_Query : constant String :=
      "INSERT OR IGNORE INTO path_hash (path, hash) VALUES (?, ?)";
   Insert_Content_Query : constant String :=
      "INSERT INTO content (hash, data) VALUES (?, ?)";

   Select_FTS_Query : constant String :=
      "SELECT rowid, crate, path, filename, hash, rank FROM f " &
      "WHERE text MATCH ? ORDER BY rank LIMIT 250";
   Select_Hash_Query : constant String :=
      "SELECT hash FROM path_hash WHERE path=? LIMIT 1";
   Select_Content_Query : constant String :=
      "SELECT data FROM content WHERE hash=? LIMIT 1";

   Select_FTS_Stmt : Sqlite.Statement;
   Insert_FTS_Stmt : Sqlite.Statement;

   Select_Hash_Stmt : Sqlite.Statement;
   Insert_Hash_Stmt : Sqlite.Statement;

   Select_Content_Stmt : Sqlite.Statement;
   Insert_Content_Stmt : Sqlite.Statement;

   DB : Sqlite.Connection;

   procedure Create is
      FTS_Query : constant String :=
         "CREATE VIRTUAL TABLE f USING fts5(crate, path, filename, hash, text)";
      Path_Hash_Query : constant String :=
         "CREATE TABLE path_hash (path TEXT, hash TEXT)";
      Content_Query : constant String :=
         "CREATE TABLE content (hash TEXT, data BLOB)";
      Stmt : Sqlite.Statement;
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      Sqlite.Initialize;
      DB := Sqlite.Open (Database_Path,
         (CREATE  => True,
          WAL     => True,
          others  => False));
      if not Sqlite.Is_Open (DB) then
         raise Program_Error with "Unable to open index.db";
      end if;
      Stmt := Sqlite.Prepare (DB, FTS_Query);
      Status := Sqlite.Step (DB, Stmt);
      if Status /= Sqlite.SQLITE_DONE then
         raise Program_Error with "Unable to create fts5 table";
      end if;
      Sqlite.Finalize (DB, Stmt);

      Stmt := Sqlite.Prepare (DB, Path_Hash_Query);
      Status := Sqlite.Step (DB, Stmt);
      if Status /= Sqlite.SQLITE_DONE then
         raise Program_Error with "Unable to create path_hash table";
      end if;
      Sqlite.Finalize (DB, Stmt);

      Stmt := Sqlite.Prepare (DB, Content_Query);
      Status := Sqlite.Step (DB, Stmt);
      if Status /= Sqlite.SQLITE_DONE then
         raise Program_Error with "Unable to create content table";
      end if;
      Sqlite.Finalize (DB, Stmt);
      Sqlite.Close (DB);
   end Create;

   procedure Open
      (Read_Only : Boolean := True)
   is
   begin
      Sqlite.Initialize;
      DB := Sqlite.Open (Database_Path,
         (READONLY => Read_Only,
          NOMUTEX  => True,
          WAL      => True,
          others   => False));
      if not Sqlite.Is_Open (DB) then
         raise Program_Error with "Unable to open index.db";
      end if;

      Select_FTS_Stmt := Sqlite.Prepare (DB, Select_FTS_Query);
      Select_Hash_Stmt := Sqlite.Prepare (DB, Select_Hash_Query);
      Select_Content_Stmt := Sqlite.Prepare (DB, Select_Content_Query);

      if not Read_Only then
         Insert_FTS_Stmt := Sqlite.Prepare (DB, Insert_FTS_Query);
         Insert_Hash_Stmt := Sqlite.Prepare (DB, Insert_Hash_Query);
         Insert_Content_Stmt := Sqlite.Prepare (DB, Insert_Content_Query);
      end if;
   end Open;

   procedure Search
      (Query   : Unicode;
       Results : out Search_Results;
       Last    : out Natural)
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;

      NUL : constant Wide_Wide_Character := Wide_Wide_Character'Val (0);
      Q : Unbounded_Unicode;
   begin
      Last := 0;

      if not Sqlite.Is_Open (DB) then
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

      Sqlite.Reset (DB, Select_FTS_Stmt);
      Sqlite.Bind_Text (DB, Select_FTS_Stmt, 1, String (Encode (Q)));

      for I in Results'Range loop
         Status := Sqlite.Step (DB, Select_FTS_Stmt);
         exit when Status /= Sqlite.SQLITE_ROW;
         Results (I) :=
            (Id         => Natural (Sqlite.To_Integer (Select_FTS_Stmt, 0)),
             Crate      => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Select_FTS_Stmt, 1)))),
             Path       => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Select_FTS_Stmt, 2)))),
             Filename   => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Select_FTS_Stmt, 3)))),
             Hash       => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Select_FTS_Stmt, 4)))),
             Rank       => Sqlite.To_Integer (Select_FTS_Stmt, 5));
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
      (Path : Unicode)
      return String
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      Sqlite.Reset (DB, Select_Hash_Stmt);
      Sqlite.Bind_Text (DB, Select_Hash_Stmt, 1, String (Encode (Path)));
      Status := Sqlite.Step (DB, Select_Hash_Stmt);
      if Status = Sqlite.SQLITE_ROW then
         return Sqlite.To_String (Select_Hash_Stmt, 0);
      elsif Status = Sqlite.SQLITE_DONE then
         return "";
      else
         raise Program_Error with "Select hash returned error: " & Status'Image;
      end if;
   end Get_Hash;

   procedure Add_Hash
      (Path : Unicode;
       Hash : String)
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      Sqlite.Reset (DB, Insert_Hash_Stmt);
      Sqlite.Bind_Text (DB, Insert_Hash_Stmt, 1, String (Encode (Path)));
      Sqlite.Bind_Text (DB, Insert_Hash_Stmt, 2, Hash);
      Status := Sqlite.Step (DB, Insert_Hash_Stmt);
      if Status /= Sqlite.SQLITE_DONE then
         raise Program_Error with "Insert path_hash failed with " & Status'Image;
      end if;
   end Add_Hash;

   function Content_Exists
      (Hash : String)
      return Boolean
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      Sqlite.Reset (DB, Select_Content_Stmt);
      Sqlite.Bind_Text (DB, Select_Content_Stmt, 1, Hash);
      Status := Sqlite.Step (DB, Select_Content_Stmt);
      return Status = Sqlite.SQLITE_ROW;
   end Content_Exists;

   procedure Add_Content
      (Hash : String;
       Data : String)
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      if not Content_Exists (Hash) then
         Sqlite.Reset (DB, Insert_Content_Stmt);
         Sqlite.Bind_Text (DB, Insert_Content_Stmt, 1, Hash);
         Sqlite.Bind_Text (DB, Insert_Content_Stmt, 2, Data);
         Status := Sqlite.Step (DB, Insert_Content_Stmt);
         if Status /= Sqlite.SQLITE_DONE then
            raise Program_Error with "Insert content failed with " & Status'Image;
         end if;
      end if;
   end Add_Content;

   function Get_Content_By_Hash
      (Hash : String)
      return String
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      Sqlite.Reset (DB, Select_Content_Stmt);
      Sqlite.Bind_Text (DB, Select_Content_Stmt, 1, Hash);
      Status := Sqlite.Step (DB, Select_Content_Stmt);
      if Status = Sqlite.SQLITE_ROW then
         return Sqlite.To_String (Select_Content_Stmt, 0);
      else
         return "";
      end if;
   end Get_Content_By_Hash;

   function Get_Text
      (Path : Unicode)
      return String
   is (Get_Content_By_Hash (Get_Hash (Path)));

   Insert_Row_Id : Natural := 0;

   procedure Add_FTS
      (Crate, Path, Filename, Hash, Text : String)
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      Sqlite.Reset (DB, Insert_FTS_Stmt);
      Sqlite.Bind_Text (DB, Insert_FTS_Stmt, 1, Insert_Row_Id'Image);
      Sqlite.Bind_Text (DB, Insert_FTS_Stmt, 2, Crate);
      Sqlite.Bind_Text (DB, Insert_FTS_Stmt, 3, Path);
      Sqlite.Bind_Text (DB, Insert_FTS_Stmt, 4, Filename);
      Sqlite.Bind_Text (DB, Insert_FTS_Stmt, 5, Hash);
      Sqlite.Bind_Text (DB, Insert_FTS_Stmt, 6, Text);
      Status := Sqlite.Step (DB, Insert_FTS_Stmt);
      if Status /= Sqlite.SQLITE_DONE then
         raise Program_Error with "Insert fts failed with " & Status'Image;
      end if;

      Insert_Row_Id := Insert_Row_Id + 1;
   end Add_FTS;

   procedure Add
      (Crate, Path, Filename, Text : String)
   is
      Hash : constant String := Content_Hash (Text);
   begin
      Add_Content (Hash, Text);
      Add_Hash (Decode (UTF8 (Path)), Hash);
      Add_FTS (Crate, Path, Filename, Hash, Text);
   end Add;

   procedure Close is
      use type Sqlite.Statement;
   begin
      Sqlite.Finalize (DB, Select_FTS_Stmt);
      Sqlite.Finalize (DB, Select_Hash_Stmt);
      Sqlite.Finalize (DB, Select_Content_Stmt);

      if Insert_FTS_Stmt /= null then
         Sqlite.Finalize (DB, Insert_FTS_Stmt);
      end if;

      if Insert_Hash_Stmt /= null then
         Sqlite.Finalize (DB, Insert_Hash_Stmt);
      end if;

      if Insert_Content_Stmt /= null then
         Sqlite.Finalize (DB, Insert_Content_Stmt);
      end if;

      Sqlite.Close (DB);
   end Close;

end Codesearch.Database;
