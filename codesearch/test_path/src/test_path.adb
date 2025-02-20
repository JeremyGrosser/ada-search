--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--
pragma Style_Checks ("M120");
with Codesearch.Path; use Codesearch.Path;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Path is
   Fail_Count : Natural := 0;

   procedure Test_Join (Left, Right, Expect : String) is
      Result : constant String := Join (Left, Right);
   begin
      if Result /= Expect then
         Put_Line ("FAIL: Join (" & Left & ", " & Right & ") /= " & Expect & " (got " & Result & " instead)");
         Fail_Count := Fail_Count + 1;
      end if;
   end Test_Join;

   procedure Test_Basename (S, Expect : String) is
      Result : constant String := Basename (S);
   begin
      if Result /= Expect then
         Put_Line ("FAIL: Basename (" & S & ") /= " & Expect & " (got " & Result & " instead)");
         Fail_Count := Fail_Count + 1;
      end if;
   end Test_Basename;

   procedure Test_Dirname (S, Expect : String) is
      Result : constant String := Dirname (S);
   begin
      if Result /= Expect then
         Put_Line ("FAIL: Dirname (" & S & ") /= " & Expect & " (got " & Result & " instead)");
         Fail_Count := Fail_Count + 1;
      end if;
   end Test_Dirname;
begin
   Test_Join ("", "", "");
   Test_Join ("/", "", "/");
   Test_Join ("", "/", "/");
   Test_Join ("foo", "/", "/");
   Test_Join ("/foo", "bar", "/foo/bar");
   Test_Join ("/foo/", "bar", "/foo/bar");
   Test_Join ("//foo", "bar", "//foo/bar");
   Test_Join ("/foo", "/bar", "/bar");

   Test_Basename ("foo", "foo");
   Test_Basename ("/foo", "foo");
   Test_Basename ("/foo/bar", "bar");

   Test_Dirname ("foo", "foo");
   Test_Dirname ("/foo", "/");
   Test_Dirname ("/foo/bar", "/foo");
   Test_Dirname ("/foo///", "/foo");

   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Fail_Count));
end Test_Path;
