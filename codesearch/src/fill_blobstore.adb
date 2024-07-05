pragma Ada_2022;
with Ada.Directories; use Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;

with Codesearch.Strings; use Codesearch.Strings;
with Codesearch.Database;
with Codesearch.Blobstore;
with SHA3;
with Hex_Format_8;

with Ada.Text_IO;

procedure Fill_Blobstore is
   Base_Dir : constant String := "source/";

   function Read_File
      (Path : String)
      return String
   is
      package IO renames Ada.Streams.Stream_IO;
      Length : constant Natural := Natural (Ada.Directories.Size (Path));
      Text   : String (1 .. Length);
      File   : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Path);
      String'Read (IO.Stream (File), Text);
      IO.Close (File);
      return Text;
   end Read_File;

   function To_String
      (Digest : SHA3.SHA3_256.Digest_Type)
      return String
   is
      use Hex_Format_8;
      S : String (1 .. Digest'Length * 2);
      I : Positive := S'First;
   begin
      for D of Digest loop
         S (I .. I + 1) := Hex (D);
         I := I + 2;
      end loop;
      return S;
   end To_String;

   function Content_Hash
      (Text : String)
      return String
   is
      use SHA3.SHA3_256;
      Input  : Byte_Array (1 .. Text'Length)
         with Import, Address => Text'Address;
      Digest : Digest_Type;
      Ctx    : Context;
   begin
      Init (Ctx);
      Update (Ctx, Input);
      Final (Ctx, Digest);
      return To_String (Digest);
   end Content_Hash;

   function Ends_With
      (S : String;
       Suffix : String)
       return Boolean
   is (S'Length >= Suffix'Length and then S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix);

   procedure Index_File
      (Full_Name : String)
   is
   begin
      if         Ends_With (Full_Name, ".ads")
         or else Ends_With (Full_Name, ".adb")
         or else Ends_With (Full_Name, ".ada")
      then
         --  Put_Line (Full_Name);
         declare
            Text : constant String := Read_File (Full_Name);
            Hash : constant String := Content_Hash (Text);
         begin
            if Text'Length = 0 then
               return;
            end if;
            if not Codesearch.Blobstore.Exists (Hash) then
               Codesearch.Blobstore.Put
                  (Id   => Hash,
                   Data => Text);
            end if;
            Codesearch.Database.Add_Hash (Decode (UTF8 (Full_Name)), Hash);
         end;
      end if;
   end Index_File;

   procedure Walk
      (Path : String)
   is
      Search : Search_Type;
      Inode : Directory_Entry_Type;
   begin
      Ada.Text_IO.Put_Line (Path);
      Start_Search (Search,
         Directory => Path,
         Pattern => "",
         Filter => (Directory => True, Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Inode);
         --  Put_Line (Inode'Image);
         case Kind (Inode) is
            when Ordinary_File =>
               Index_File (Full_Name (Inode));
            when Directory =>
               if Simple_Name (Inode) (1) /= '.' then
                  Walk (Full_Name (Inode));
               end if;
            when Special_File =>
               null;
         end case;
      end loop;
      End_Search (Search);
   end Walk;
begin
   Codesearch.Database.Open (Read_Only => False);
   Walk (Base_Dir);
   Codesearch.Database.Close;
end Fill_Blobstore;
