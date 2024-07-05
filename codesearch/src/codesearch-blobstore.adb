with Codesearch.Path;
with Ada.Directories;
with Ada.Streams.Stream_IO;

package body Codesearch.Blobstore is

   Root : constant String := "blobstore/";

   function To_Path
      (Id : String)
      return String
   is
      use Codesearch.Path;
   begin
      return "./" & Join (Join (Join (Root, "" & Id (Id'First)), "" & Id (Id'First + 1)), Id (Id'First + 2 .. Id'Last));
   end To_Path;

   procedure Makedirs
      (Path : String)
   is
   begin
      for I in Path'Range loop
         if Path (I) = '/' then
            declare
               Dir : constant String := Path (Path'First .. I - 1);
            begin
               if not Ada.Directories.Exists (Dir) then
                  Ada.Directories.Create_Directory (Dir);
               end if;
            end;
         end if;
      end loop;
   end Makedirs;

   procedure Put
      (Id   : String;
       Data : String)
   is
      use Ada.Streams.Stream_IO;
      Path  : constant String := To_Path (Id);
      File  : File_Type;
   begin
      Makedirs (Path);
      Create (File, Out_File, Path);
      String'Write (Stream (File), Data);
      Close (File);
   end Put;

   function Length
      (Id : String)
      return Natural
   is (Natural (Ada.Directories.Size (To_Path (Id))));

   function Exists
      (Id : String)
      return Boolean
   is (Ada.Directories.Exists (To_Path (Id)));

   procedure Get
      (Id   : String;
       Data : out String)
   is
      use Ada.Streams.Stream_IO;
      Path : constant String := To_Path (Id);
      File : File_Type;
   begin
      Open (File, In_File, Path);
      String'Read (Stream (File), Data);
      Close (File);
   end Get;

   function Get
      (Id : String)
      return String
   is
      Data : String (1 .. Length (Id));
   begin
      Get (Id, Data);
      return Data;
   end Get;

end Codesearch.Blobstore;
