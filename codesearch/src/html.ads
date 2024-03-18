package HTML is

   function Escape
      (Text  : String;
       Quote : Boolean := True)
      return String;

   function Unescape
      (Text : String)
      return String;

end HTML;
