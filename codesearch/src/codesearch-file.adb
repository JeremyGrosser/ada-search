--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
with Ada.Directories;
with Ada.Streams.Stream_IO;

package body Codesearch.File is

   procedure Set_Working_Directory is
   begin
      Ada.Directories.Set_Directory (Share.Prefix_Path);
   end Set_Working_Directory;

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

   function Resource_Exists
      (Filename : String)
      return Boolean
   is (Ada.Directories.Exists (Share.Resource_Path & Filename));

end Codesearch.File;
