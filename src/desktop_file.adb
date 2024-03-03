with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling;

package body Desktop_File is

   use type String_Vectors.Vector; -- make "&" visible

   Categories : constant String_Vectors.Vector := String_Vectors.Empty_Vector &
      ("AudioVideo") &
      -- A multimedia (audio/video) application

      ("Audio") &
      -- An audio application
      -- Desktop entry must include AudioVideo as well

      ("Video") &
      -- A video application
      -- Desktop entry must include AudioVideo as well

      ("Development") &
      -- An application for development

      ("Education") &
      -- Educational software

      ("Game") &
      -- A game

      ("Graphics") &
      -- Graphical application

      ("Network") &
      -- Network application such as a web browser

      ("Office") &
      -- An office type application

      ("Settings") &
      -- Settings applications
      -- Entries may appear in a separate menu or as part of a "Control Center"

      ("System") &
      -- System (application) & "System Tools" such as say a log viewer or network monitor

      ("Utility")
     -- Small utility application, "Accessories"
     ;

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
      Tags : String_Vectors.Vector) is

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

      Put (Desktop_File, "Keywords=");
      for Tag of Tags loop
         Put (Desktop_File, Tag & ";");
      end loop;
      New_Line (Desktop_File);

      Put (Desktop_File, "Categories=");
      for Tag of Tags loop
         for Each_Category of Categories loop
            if Ada.Strings.Equal_Case_Insensitive (Each_Category, Tag)
            then
               Put (Desktop_File, Each_Category & ";");
            end if;
         end loop;
      end loop;
      New_Line (Desktop_File);

      Close (Desktop_File);

   end Write;

end Desktop_File;
