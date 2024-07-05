package Codesearch.Blobstore
   with Elaborate_Body
is

   procedure Put
      (Id   : String;
       Data : String);

   function Get
      (Id : String)
      return String;

   function Exists
      (Id : String)
      return Boolean;

end Codesearch.Blobstore;
