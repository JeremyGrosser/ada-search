with Sqlite;

package body Codesearch.Database is

   Database_Path : constant String := "/home/synack/src/ada-search/codesearch/index.db";

   Insert_FTS_Query : constant String :=
      "INSERT INTO f(rowid, crate, path, filename, hash, text) VALUES (?, ?, ?, ?, ?, ?)";
   Insert_Hash_Query : constant String :=
      "INSERT OR IGNORE INTO path_hash (path, hash) VALUES (?, ?)";

   Select_FTS_Query : constant String :=
      "SELECT rowid, crate, path, filename, hash, rank FROM f " &
      "WHERE text MATCH ? GROUP BY crate, filename ORDER BY rank LIMIT 250";
   Select_Hash_Query : constant String :=
      "SELECT hash FROM path_hash WHERE path=? LIMIT 1";

   Select_FTS_Stmt : Sqlite.Statement;
   Insert_FTS_Stmt : Sqlite.Statement;

   Insert_Hash_Stmt : Sqlite.Statement;

   DB : Sqlite.Connection;

   procedure Create is
      FTS_Query : constant String :=
         "CREATE VIRTUAL TABLE f USING fts5(crate, path, filename, hash, text)";
      Path_Hash_Query : constant String :=
         "CREATE TABLE path_hash (path TEXT, hash TEXT)";
      Stmt : Sqlite.Statement;
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      Sqlite.Initialize;
      DB := Sqlite.Open (Database_Path,
         (CREATE => True,
          others => False));
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
          others   => False));
      if not Sqlite.Is_Open (DB) then
         raise Program_Error with "Unable to open index.db";
      end if;
      Select_FTS_Stmt := Sqlite.Prepare (DB, Select_FTS_Query);
      if not Read_Only then
         Insert_FTS_Stmt := Sqlite.Prepare (DB, Insert_FTS_Query);
         Insert_Hash_Stmt := Sqlite.Prepare (DB, Insert_Hash_Query);
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

   function Get_Hash
      (Path : Unicode)
      return String
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
      Stmt   : Sqlite.Statement := Sqlite.Prepare (DB, Select_Hash_Query);
   begin
      Sqlite.Reset (DB, Stmt);
      Sqlite.Bind_Text (DB, Stmt, 1, String (Encode (Path)));
      Status := Sqlite.Step (DB, Stmt);
      if Status = Sqlite.SQLITE_ROW then
         declare
            Hash : constant String := Sqlite.To_String (Stmt, 0);
         begin
            Sqlite.Finalize (DB, Stmt);
            return Hash;
         end;
      elsif Status = Sqlite.SQLITE_DONE then
         Sqlite.Finalize (DB, Stmt);
         return "";
      else
         Sqlite.Finalize (DB, Stmt);
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

   Insert_Row_Id : Natural := 0;

   procedure Add
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

      Add_Hash (Decode (UTF8 (Path)), Hash);

      Insert_Row_Id := Insert_Row_Id + 1;
   end Add;

   procedure Close is
      use type Sqlite.Statement;
   begin
      Sqlite.Finalize (DB, Select_FTS_Stmt);
      if Insert_FTS_Stmt /= null then
         Sqlite.Finalize (DB, Insert_FTS_Stmt);
      end if;

      if Insert_Hash_Stmt /= null then
         Sqlite.Finalize (DB, Insert_Hash_Stmt);
      end if;
      Sqlite.Close (DB);
   end Close;

end Codesearch.Database;
