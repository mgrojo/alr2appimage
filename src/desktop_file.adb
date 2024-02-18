with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling;

package body Desktop_File is

   type Categories is
     (
      AudioVideo,
      -- A multimedia (audio/video) application

      Audio,
      -- An audio application
      -- Desktop entry must include AudioVideo as well

      Video,
      -- A video application
      -- Desktop entry must include AudioVideo as well

      Development,
      -- An application for development

      Education,
      -- Educational software

      Game,
      -- A game

      Graphics,
      -- Graphical application

      Network,
      -- Network application such as a web browser

      Office,
      -- An office type application

      Settings,
      -- Settings applications
      -- Entries may appear in a separate menu or as part of a "Control Center"

      System,
      -- System application, "System Tools" such as say a log viewer or network monitor

      Utility
     -- Small utility application, "Accessories"
     );

   procedure Put_Value
     (File : File_Type;
      Key, Value : String) is
   begin
      Put_Line (File, Key & "=" & Value);
   end Put_Value;

   procedure Put_Value
     (File : File_Type;
      Key : String;
      Value : Boolean) is
   begin
      Put_Line (File, Key & "=" &
                  Ada.Characters.Handling.To_Lower (Value'Image));
   end Put_Value;

   procedure Write
     (Name : String;
      Comment : String;
      Exec : String;
      Icon : String;
      Terminal : Boolean;
      Tags : String_Array) is

      Desktop_File : File_Type;
   begin
      Create (File => Desktop_File, Name => Name & ".desktop");

      Put_Line (Desktop_File, "[Desktop Entry]");

      Put_Value (Desktop_File, "Name", Name);
      Put_Value (Desktop_File, "Comment", Comment);
      Put_Value (Desktop_File, "Exec", Exec);
      Put_Value (Desktop_File, "Icon", Icon);
      Put_Value (Desktop_File, "Terminal", Terminal);

      Put_Value (Desktop_File, "Type", "Application");

      Put (Desktop_File, "Categories=");

      for Tag of Tags loop
         Put (Desktop_File, -Tag & ";");
      end loop;

      Close (Desktop_File);

   end Write;

end Desktop_File;
