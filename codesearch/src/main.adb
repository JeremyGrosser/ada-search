with Codesearch.Database;
with Codesearch.Web;

procedure Main is
begin
   Codesearch.Database.Initialize;
   Codesearch.Web.Do_Request;
   Codesearch.Database.Close;
end Main;
