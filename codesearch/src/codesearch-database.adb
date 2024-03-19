with Sqlite;

package body Codesearch.Database is

   Query : constant String :=
      "SELECT crate, filename, path, rank FROM f WHERE text MATCH ? GROUP BY crate, filename ORDER BY rank LIMIT 250";

   DB : Sqlite.Connection;
   Stmt : Sqlite.Statement;

   procedure Initialize is
   begin
      Sqlite.Initialize;
      DB := Sqlite.Open ("/home/synack/src/ada-search/codesearch/index.db",
         (READONLY => True,
          NOMUTEX  => True,
          others   => False));
      if not Sqlite.Is_Open (DB) then
         raise Program_Error with "Unable to open index.db";
      end if;
      Stmt := Sqlite.Prepare (DB, Query);
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

      Sqlite.Reset (DB, Stmt);
      Sqlite.Bind_Text (DB, Stmt, 1, String (Encode (Q)));

      for I in Results'Range loop
         Status := Sqlite.Step (DB, Stmt);
         exit when Status /= Sqlite.SQLITE_ROW;
         Results (I) :=
            (Crate      => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Stmt, 0)))),
             Filename   => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Stmt, 1)))),
             Path       => To_Unbounded (Decode (UTF8 (Sqlite.To_String (Stmt, 2)))),
             Rank       => Sqlite.To_Integer (Stmt, 3));
         Last := I;
      end loop;

      case Status is
         when Sqlite.SQLITE_ROW | Sqlite.SQLITE_DONE =>
            null;
         when others =>
            raise Program_Error with "Query returned error: " & Status'Image;
      end case;
   end Search;

   procedure Close is
   begin
      Sqlite.Finalize (DB, Stmt);
      Sqlite.Close (DB);
   end Close;

end Codesearch.Database;
