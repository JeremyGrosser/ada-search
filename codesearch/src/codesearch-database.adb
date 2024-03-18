with Codesearch.Strings;
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
      (Query   : String;
       Results : out Search_Results;
       Last    : out Natural)
   is
      use type Sqlite.Result_Code;
      Status : Sqlite.Result_Code;

      Quote : constant Virtual_String := Codesearch.Strings.UTF8_Decode ("""");
      Q : Virtual_String := Codesearch.Strings.UTF8_Decode (Query);
   begin
      Last := 0;

      if not Sqlite.Is_Open (DB) then
         raise Program_Error with "Database not open";
      end if;

      Prepend (Q, Quote);
      Append (Q, Quote);

      Sqlite.Reset (DB, Stmt);
      Sqlite.Bind_Text (DB, Stmt, 1, Codesearch.Strings.UTF8_Encode (Q));

      for I in Results'Range loop
         Status := Sqlite.Step (DB, Stmt);
         exit when Status /= Sqlite.SQLITE_ROW;
         Results (I) :=
            (Crate      => Codesearch.Strings.UTF8_Decode (Sqlite.To_String (Stmt, 0)),
             Filename   => Codesearch.Strings.UTF8_Decode (Sqlite.To_String (Stmt, 1)),
             Path       => Codesearch.Strings.UTF8_Decode (Sqlite.To_String (Stmt, 2)),
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
