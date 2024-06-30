with Codesearch.Database;
with Codesearch.Web;

procedure Main is
begin
   Codesearch.Database.Open (Read_Only => False);
   Codesearch.Web.Do_Request;
   Codesearch.Database.Close;
end Main;
