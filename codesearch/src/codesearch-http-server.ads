with Codesearch.Database;

package Codesearch.HTTP.Server is

   procedure Bind;
   procedure Run
      (DB : Codesearch.Database.Session);
   procedure Stop;

end Codesearch.HTTP.Server;
