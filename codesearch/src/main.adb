with Codesearch.HTTP.Server;
with Codesearch.Database;
with Codesearch.File;
with GNAT.Ctrl_C;

procedure Main is
   package Server renames Codesearch.HTTP.Server;

   procedure Stop is
   begin
      Server.Stop;
   end Stop;

   DB : Codesearch.Database.Session;
begin
   Codesearch.File.Set_Working_Directory;
   DB := Codesearch.Database.Open (Read_Only => True);
   Server.Bind;
   GNAT.Ctrl_C.Install_Handler (Stop'Unrestricted_Access);
   Server.Run (DB);
   Codesearch.Database.Close (DB);
end Main;
