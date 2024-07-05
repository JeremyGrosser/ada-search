with Ada.Directories;
with Ada.Streams.Stream_IO;
with Resources;
with Codesearch_Config;

package body Codesearch.File is
   package Share is new Resources
      (Crate_Name => Codesearch_Config.Crate_Name);

   function Read_Unicode
      (Filename : String)
      return Unicode
   is
      use Ada.Streams.Stream_IO;
      F : Ada.Streams.Stream_IO.File_Type;
      Data : UTF8 (1 .. Natural (Ada.Directories.Size (Filename)));
   begin
      Open (F, In_File, Filename);
      UTF8'Read (Stream (F), Data);
      Close (F);
      return Decode (Data);
   end Read_Unicode;

   function Read_Resource
      (Filename : String)
      return Unicode
   is
   begin
      return Read_Unicode (Share.Resource_Path & Filename);
   end Read_Resource;

end Codesearch.File;
