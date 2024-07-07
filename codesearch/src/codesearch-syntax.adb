pragma Ada_2022;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Analysis;
with Ada.Text_IO;

package body Codesearch.Syntax is

   package LAL renames Libadalang.Analysis;

   type Token_CSS is
      (CSS_Default,
       CSS_Error,
       CSS_Comment_Single,
       CSS_Keyword_Constant,
       CSS_Keyword_Declaration,
       CSS_Keyword_Namespace,
       CSS_Keyword_Reserved,
       CSS_Keyword_Type,
       CSS_Operator,
       CSS_Operator_Word,
       CSS_Punctuation,
       CSS_Name,
       CSS_Name_Attribute,
       CSS_Name_Class,
       CSS_Name_Constant,
       CSS_Name_Exception,
       CSS_Name_Function,
       CSS_Name_Label,
       CSS_Name_Variable,
       CSS_Number_Float,
       CSS_Number_Integer,
       CSS_String,
       CSS_String_Char);

   type Style is array (Token_Kind) of Token_CSS;

   Tango : constant Style :=
      (Ada_Termination     => CSS_Default,
       Ada_Lexing_Failure  => CSS_Error,
       Ada_Identifier      => CSS_Name,
       Ada_All             => CSS_Keyword_Reserved,
       Ada_Abort           => CSS_Keyword_Reserved,
       Ada_Else            => CSS_Keyword_Reserved,
       Ada_New             => CSS_Keyword_Reserved,
       Ada_Return          => CSS_Keyword_Reserved,
       Ada_Abs             => CSS_Operator_Word,
       Ada_Elsif           => CSS_Keyword_Reserved,
       Ada_Not             => CSS_Operator_Word,
       Ada_Reverse         => CSS_Keyword_Reserved,
       Ada_End             => CSS_Keyword_Reserved,
       Ada_Null            => CSS_Keyword_Constant,
       Ada_Accept          => CSS_Keyword_Reserved,
       Ada_Entry           => CSS_Keyword_Reserved,
       Ada_Select          => CSS_Keyword_Reserved,
       Ada_Access          => CSS_Keyword_Reserved,
       Ada_Exception       => CSS_Keyword_Reserved,
       Ada_Of              => CSS_Keyword_Reserved,
       Ada_Separate        => CSS_Keyword_Reserved,
       Ada_Exit            => CSS_Keyword_Reserved,
       Ada_Or              => CSS_Operator_Word,
       Ada_Others          => CSS_Keyword_Reserved,
       Ada_Subtype         => CSS_Keyword_Declaration,
       Ada_And             => CSS_Operator_Word,
       Ada_For             => CSS_Keyword_Reserved,
       Ada_Out             => CSS_Keyword_Reserved,
       Ada_Array           => CSS_Keyword_Reserved,
       Ada_Function        => CSS_Keyword_Declaration,
       Ada_At              => CSS_Keyword_Reserved,
       Ada_Generic         => CSS_Keyword_Declaration,
       Ada_Package         => CSS_Keyword_Declaration,
       Ada_Task            => CSS_Keyword_Declaration,
       Ada_Begin           => CSS_Keyword_Reserved,
       Ada_Goto            => CSS_Keyword_Reserved,
       Ada_Pragma          => CSS_Keyword_Reserved,
       Ada_Terminate       => CSS_Keyword_Reserved,
       Ada_Body            => CSS_Keyword_Declaration,
       Ada_Private         => CSS_Keyword_Declaration,
       Ada_Then            => CSS_Operator_Word,
       Ada_If              => CSS_Keyword_Reserved,
       Ada_Procedure       => CSS_Keyword_Declaration,
       Ada_Type            => CSS_Keyword_Declaration,
       Ada_Case            => CSS_Keyword_Reserved,
       Ada_In              => CSS_Keyword_Reserved,
       Ada_Constant        => CSS_Name_Constant,
       Ada_Is              => CSS_Keyword_Reserved,
       Ada_Raise           => CSS_Keyword_Reserved,
       Ada_Use             => CSS_Keyword_Namespace,
       Ada_Declare         => CSS_Keyword_Reserved,
       Ada_Range           => CSS_Keyword_Type,
       Ada_Delay           => CSS_Keyword_Reserved,
       Ada_Limited         => CSS_Keyword_Type,
       Ada_Record          => CSS_Keyword_Reserved,
       Ada_When            => CSS_Keyword_Reserved,
       Ada_Delta           => CSS_Keyword_Type,
       Ada_Loop            => CSS_Keyword_Reserved,
       Ada_Rem             => CSS_Operator_Word,
       Ada_While           => CSS_Keyword_Reserved,
       Ada_Digits          => CSS_Keyword_Type,
       Ada_Renames         => CSS_Keyword_Reserved,
       Ada_Do              => CSS_Keyword_Reserved,
       Ada_Mod             => CSS_Operator_Word,
       Ada_Xor             => CSS_Operator_Word,
       Ada_Par_Close       => CSS_Punctuation,
       Ada_Par_Open        => CSS_Punctuation,
       Ada_Brack_Close     => CSS_Punctuation,
       Ada_Brack_Open      => CSS_Punctuation,
       Ada_Semicolon       => CSS_Punctuation,
       Ada_Colon           => CSS_Punctuation,
       Ada_Comma           => CSS_Punctuation,
       Ada_Doubledot       => CSS_Punctuation,
       Ada_Dot             => CSS_Punctuation,
       Ada_Diamond         => CSS_Punctuation,
       Ada_Lte             => CSS_Operator,
       Ada_Gte             => CSS_Operator,
       Ada_Arrow           => CSS_Punctuation,
       Ada_Equal           => CSS_Operator,
       Ada_Lt              => CSS_Operator,
       Ada_Gt              => CSS_Operator,
       Ada_Plus            => CSS_Operator,
       Ada_Minus           => CSS_Operator,
       Ada_Power           => CSS_Operator,
       Ada_Mult            => CSS_Operator,
       Ada_Amp             => CSS_Operator,
       Ada_Notequal        => CSS_Operator,
       Ada_Divide          => CSS_Operator,
       Ada_Tick            => CSS_Operator,
       Ada_Pipe            => CSS_Operator,
       Ada_Assign          => CSS_Punctuation,
       Ada_Label_Start     => CSS_Name_Label,
       Ada_Label_End       => CSS_Name_Label,
       Ada_Target          => CSS_Punctuation,
       Ada_String          => CSS_String,
       Ada_Char            => CSS_String_Char,
       Ada_With            => CSS_Keyword_Namespace,
       Ada_Decimal         => CSS_Number_Float,
       Ada_Integer         => CSS_Number_Integer,
       Ada_Comment         => CSS_Comment_Single,
       Ada_Prep_Line       => CSS_Default,
       Ada_Whitespace      => CSS_Default);

   function CSS_Class
      (C : Token_CSS)
      return Unicode
   is
   begin
      case C is
         when CSS_Error                => return "err";
         when CSS_Comment_Single       => return "c1";
         when CSS_Keyword_Constant     => return "kc";
         when CSS_Keyword_Declaration  => return "kd";
         when CSS_Keyword_Namespace    => return "kn";
         when CSS_Keyword_Reserved     => return "kr";
         when CSS_Keyword_Type         => return "kt";
         when CSS_Operator             => return "o";
         when CSS_Operator_Word        => return "ow";
         when CSS_Punctuation          => return "p";
         when CSS_Name                 => return "n";
         when CSS_Name_Attribute       => return "na";
         when CSS_Name_Class           => return "nc";
         when CSS_Name_Constant        => return "no";
         when CSS_Name_Exception       => return "ne";
         when CSS_Name_Function        => return "nf";
         when CSS_Name_Label           => return "nl";
         when CSS_Name_Variable        => return "nv";
         when CSS_Number_Float         => return "mf";
         when CSS_Number_Integer       => return "mi";
         when CSS_String               => return "s";
         when CSS_String_Char          => return "s1";
         when CSS_Default              => return "";
      end case;
   end CSS_Class;

   procedure Put_Token
      (R : in out Unbounded_Unicode;
       T : Token_Reference)
   is
      K   : constant Token_Kind := Kind (Data (T));
      CSS : constant Unicode := CSS_Class (Tango (K));
   begin
      Ada.Text_IO.Put_Line (Data (T)'Image);

      if CSS /= "" then
         Append (R, "<span class=""");
         Append (R, CSS);
         Append (R, " tt-");
         Append (R, K'Wide_Wide_Image);
         Append (R, """>");
         Append (R, Text (T));
         Append (R, "</span>");
      else
         Append (R, Text (T));
      end if;
   end Put_Token;

   function Highlight
      (Text : String)
      return Unicode
   is
      R : Unbounded_Unicode;
      Context  : constant LAL.Analysis_Context := LAL.Create_Context;
      Unit     : constant LAL.Analysis_Unit := LAL.Get_From_Buffer
         (Context  => Context,
          Filename => "",
          Buffer   => Text);
   begin
      Append (R, "<div class=""source"">");
      if LAL.Has_Diagnostics (Unit) then
         for Diag of LAL.Diagnostics (Unit) loop
            Append (R, "<p class=""err"">");
            Append (R, Decode (UTF8 (LAL.Format_GNU_Diagnostic (Unit, Diag))));
            Append (R, "</p>");
         end loop;
      end if;

      Append (R, "<pre>");

      declare
         Token : Token_Reference := LAL.First_Token (Unit);
      begin
         while Token /= No_Token loop
            Put_Token (R, Token);
            Token := Next (Token);
         end loop;
      end;

      Append (R, "</pre>");
      Append (R, "</div>");
      return To_Unicode (R);
   end Highlight;
end Codesearch.Syntax;
