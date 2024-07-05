with Codesearch.HTTP;

package Codesearch.Service is

   package HTTP renames Codesearch.HTTP;

   procedure Handle_Request
      (Request  : HTTP.Request;
       Response : in out HTTP.Response);

end Codesearch.Service;
