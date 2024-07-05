pragma Ada_2022;
with Ada.Directories; use Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;

with Codesearch.Database;
with Codesearch.Blobstore;
with SHA3;
with Hex_Format_8;

procedure Build_Index is
   Base_Dir : constant String := "source/alire-20240227/";

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

   procedure Index_File
      (Full_Name : String)
   is
   begin
      if Full_Name'Length >= 4 and then Full_Name (Full_Name'Last - 3 .. Full_Name'Last) = ".ads" then
         --  Put_Line (Full_Name);
         declare
            Trimmed : constant String := Full_Name (Full_Name'First + Base_Dir'Length .. Full_Name'Last);
            Crate   : constant String := Trimmed (Trimmed'First .. Ada.Strings.Fixed.Index (Trimmed, "/") - 1);
            Text    : constant String := Read_File (Full_Name);
            Hash    : constant String := Content_Hash (Text);
         begin
            Codesearch.Database.Add
               (Crate    => Crate,
                Path     => Full_Name,
                Filename => Simple_Name (Full_Name),
                Hash     => Hash,
                Text     => Text);
            if not Codesearch.Blobstore.Exists (Hash) then
               Codesearch.Blobstore.Put
                  (Id   => Hash,
                   Data => Text);
            end if;
         end;
      end if;
   end Index_File;

   procedure Walk
      (Path : String)
   is
      Search : Search_Type;
      Inode : Directory_Entry_Type;
   begin
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
   Codesearch.Database.Create;
   Codesearch.Database.Open (Read_Only => False);
   Walk (Base_Dir);
   Codesearch.Database.Close;
end Build_Index;
