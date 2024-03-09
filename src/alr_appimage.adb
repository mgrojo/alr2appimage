with Ada.Command_Line;
with Ada.Containers;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Alire_TOML;
with Alr_Appimage_Config;
with Desktop_File;
with Runner;
with String_Vectors;

with Parse_Args;
with Resources;

procedure Alr_Appimage is

   use Parse_Args;

   procedure Report_Failure (Message : String) is
   begin
      Put_Line (File => Standard_Error, Item => Message);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Report_Failure;

   package My_Resources is new Resources (Alr_Appimage_Config.Crate_Name);

   AP : Argument_Parser;
   Success : Boolean;
begin

   AP.Add_Option (Make_Boolean_Option (False), "help", 'h',
                  Usage => "Display this help text.");

   AP.Add_Option (Make_Boolean_Option (False), "version", 'V',
                  Usage => "Display the version of this utility.");

   AP.Add_Option (Make_Boolean_Option (False), "terminal", 't',
                  Usage => "Set the Terminal flag of the AppImage to true, "
                    & "i.e. the application is for the terminal or requires "
                    & "to be run from a terminal.");

   AP.Add_Option (Make_String_Option (My_Resources.Resource_Path & "alr_appimage.png"),
                  "icon", 'i',
                  Usage => "Specify the icon file for the AppImage");

   AP.Set_Prologue ("Makes an AppImage from your Alire crate.");

   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value ("help") then
      AP.Usage;
      return;

   elsif AP.Parse_Success and then AP.Boolean_Value ("version") then
      Put_Line (Alr_Appimage_Config.Crate_Name & " version: " & Alr_Appimage_Config.Crate_Version);
      return;

   elsif not AP.Parse_Success then
      Report_Failure ("Error while parsing command-line arguments: " & AP.Parse_Message);
      return;
   end if;

   Alire_TOML.Load_Alire;

   Write_Desktop :
   declare
      Executable_List : constant String_Vectors.Vector
        := Alire_TOML.Read_String_List ("executables");
   begin
      if Ada.Containers."=" (Executable_List.Length, 0) then
         Report_Failure ("Error: the field 'executables' in 'alire.toml' is empty");
         return;
      end if;

      Desktop_File.Write
        (Name => Alire_TOML.Read_Field ("name"),
         Comment => Alire_TOML.Read_Field ("description"),
         Exec => Executable_List.Element (Positive'First),
         Icon => Ada.Directories.Base_Name (AP.String_Value ("icon")),
         Terminal => AP.Boolean_Value ("terminal"),
         Tags => Alire_TOML.Read_String_List ("tags"));
   end Write_Desktop;

   Runner.Run_Alr_Install (Icon => AP.String_Value ("icon"), Success => Success);

   if not Success then
      Put_Line (Standard_Error, "Running ""alr install"" Failed. Aborting.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Runner.Run_Get_Linuxdeploy (Use_Tool => Runner.Curl, Success => Success);

   if not Success then
      Runner.Run_Get_Linuxdeploy (Use_Tool => Runner.Wget, Success => Success);

      if not Success then
         Report_Failure ("Downloading linuxdeploy failed. Please, install wget or curl.");
         return;
      end if;
   end if;

   Runner.Run_Linuxdeploy (Executable => Alire_TOML.Read_Field ("name"),
                           Icon_File => Ada.Directories.Simple_Name (AP.String_Value ("icon")),
                           Success => Success);
   if not Success then
      Report_Failure ("Running linuxdeploy failed. Aborting.");
      return;
   else
      Put_Line ("Success! AppImage is ready in: " &
                  Alire_TOML.Read_Field ("name") & "-*.AppImage");
   end if;

end Alr_Appimage;
