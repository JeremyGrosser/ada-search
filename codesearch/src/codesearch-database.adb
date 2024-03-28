with Sqlite;

package body Codesearch.Database is

   Database_Path : constant String := "/home/synack/src/ada-search/codesearch/index.db";

   Select_Query : constant String :=
      "SELECT crate, filename, path, rank FROM f WHERE text MATCH ? GROUP BY crate, filename ORDER BY rank LIMIT 250";
   Insert_Query : constant String :=
      "INSERT INTO f(rowid, crate, path, filename, text) VALUES (?, ?, ?, ?, ?)";

   Select_Stmt : Sqlite.Statement;
   Insert_Stmt : Sqlite.Statement;
   DB : Sqlite.Connection;

   procedure Create is
      Query : constant String :=
         "CREATE VIRTUAL TABLE f USING fts5(crate, path, filename, text)";
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
      Stmt := Sqlite.Prepare (DB, Query);
      Status := Sqlite.Step (DB, Stmt);
      if Status /= Sqlite.SQLITE_DONE then
         raise Program_Error with "Unable to create fts5 table";
      end if;
      Sqlite.Finalize (DB, Stmt);
      Sqlite.Close (DB);
   end Create;

   procedure Initialize is
   begin
      Sqlite.Initialize;
      DB := Sqlite.Open (Database_Path,
         (READONLY => False,
          NOMUTEX  => True,
          others   => False));
      if not Sqlite.Is_Open (DB) then
         raise Program_Error with "Unable to open index.db";
      end if;
      Select_Stmt := Sqlite.Prepare (DB, Select_Query);
      Insert_Stmt := Sqlite.Prepare (DB, Insert_Query);
   end Initialize;

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

      Sqlite.Reset (DB, Select_Stmt);
      Sqlite.Bind_Text (DB, Select_Stmt, 1, String (Encode (Q)));

      for I in Results'Range loop
         Status := Sqlite.Step (DB, Select_Stmt);
         exit when Status /= Sqlite.SQLITE_ROW;
         Results (I) :=
            (Crate      => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Select_Stmt, 0)))),
             Filename   => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Select_Stmt, 1)))),
             Path       => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Select_Stmt, 2)))),
             Rank       => Sqlite.To_Integer (Select_Stmt, 3));
         Last := I;
      end loop;

      case Status is
         when Sqlite.SQLITE_ROW | Sqlite.SQLITE_DONE =>
            null;
         when others =>
            raise Program_Error with "Query returned error: " & Status'Image;
      end case;
   end Search;

   Insert_Row_Id : Natural := 0;

   procedure Add
      (Crate, Path, Filename, Text : String)
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;
   begin
      Sqlite.Reset (DB, Insert_Stmt);
      Sqlite.Bind_Text (DB, Insert_Stmt, 1, Insert_Row_Id'Image);
      Sqlite.Bind_Text (DB, Insert_Stmt, 2, Crate);
      Sqlite.Bind_Text (DB, Insert_Stmt, 3, Path);
      Sqlite.Bind_Text (DB, Insert_Stmt, 4, Filename);
      Sqlite.Bind_Text (DB, Insert_Stmt, 5, Text);
      Status := Sqlite.Step (DB, Insert_Stmt);
      if Status /= Sqlite.SQLITE_DONE then
         raise Program_Error with "Insert failed with " & Status'Image;
      end if;

      Insert_Row_Id := Insert_Row_Id + 1;
   end Add;

   procedure Close is
   begin
      Sqlite.Finalize (DB, Select_Stmt);
      Sqlite.Finalize (DB, Insert_Stmt);
      Sqlite.Close (DB);
   end Close;

end Codesearch.Database;
