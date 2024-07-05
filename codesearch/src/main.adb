with Codesearch.HTTP.Server;

procedure Main is
   package Server renames Codesearch.HTTP.Server;
begin
   Server.Bind;
   Server.Run;
end Main;
