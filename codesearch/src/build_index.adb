with Ada.Directories; use Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Codesearch.Database;
with Codesearch.File;
with Ada.Text_IO;

procedure Build_Index is
   Base_Dir : constant String := "source/alire-20241219/";
   DB : Codesearch.Database.Session;

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

   function Ends_With
      (S : String;
       Suffix : String)
       return Boolean
   is (S'Length >= Suffix'Length and then S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix);

   function Should_Index
      (Name : String)
      return Boolean
   is (Ends_With (Name, ".ads") or else
       Ends_With (Name, ".adb") or else
       Ends_With (Name, ".ada"));

   procedure Index_File
      (Full_Name : String)
   is
   begin
      if Should_Index (Full_Name) then
         Ada.Text_IO.Put_Line (Full_Name);
         declare
            Trimmed : constant String := Full_Name (Full_Name'First + Base_Dir'Length .. Full_Name'Last);
            Crate   : constant String := Trimmed (Trimmed'First .. Ada.Strings.Fixed.Index (Trimmed, "/") - 1);
            Text    : constant String := Read_File (Full_Name);
         begin
            if Text'Length = 0 then
               return;
            end if;

            Codesearch.Database.Add
               (This     => DB,
                Crate    => Crate,
                Path     => Full_Name,
                Filename => Simple_Name (Full_Name),
                Text     => Text);
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
   Codesearch.File.Set_Working_Directory;
   Codesearch.Database.Create;
   DB := Codesearch.Database.Open (Read_Only => False);
   Walk (Base_Dir);
   Codesearch.Database.Close (DB);
end Build_Index;
