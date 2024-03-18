with Ada.Directories;
with Ada.Streams.Stream_IO;

package body Codesearch.File is

   function Exists
      (Filename : String)
      return Boolean
   is (Ada.Directories.Exists (Filename));

   function Length
      (Filename : String)
      return Natural
   is (Natural (Ada.Directories.Size (Filename)));

   procedure Read
      (Filename : String;
       Data : out String)
   is
      use Ada.Streams.Stream_IO;
      F : File_Type;
   begin
      Open (F, In_File, Filename);
      String'Read (Stream (F), Data);
      Close (F);
   end Read;

end Codesearch.File;
