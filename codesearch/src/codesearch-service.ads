with Codesearch.HTTP;
with Codesearch.Database;

package Codesearch.Service is

   package HTTP renames Codesearch.HTTP;

   procedure Handle_Request
      (Request  : HTTP.Request;
       Response : in out HTTP.Response;
       DB       : Codesearch.Database.Session);

end Codesearch.Service;
