with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Alire_TOML;
with Desktop_File;
with Runner;

with Parse_Args;
use Parse_Args;

procedure Alr_Appimage is
   AP : Argument_Parser;
   Success : Boolean;
begin

   AP.Add_Option (Make_Boolean_Option (False), "help", 'h',
                  Usage => "Display this help text");

   AP.Add_Option (Make_String_Option ("alr_appimage.png"), "icon", 'i',
                  Usage => "Specify the icon file for the AppImage");

   AP.Set_Prologue ("Makes an AppImage from your Alire crate.");

   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value ("help") then
      AP.Usage;
      return;

   elsif not AP.Parse_Success then
      Put_Line (Standard_Error,
               "Error while parsing command-line arguments: " & AP.Parse_Message);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Alire_TOML.Load_Alire;

   Desktop_File.Write
     (Name => Alire_TOML.Read_Field ("name"),
      Comment => Alire_TOML.Read_Field ("description"),
      Exec => Alire_TOML.Read_Field ("name"), -- TODO: Alire_TOML.Read_Field ("executables"),
      Icon => Ada.Directories.Base_Name (AP.String_Value ("icon")),
      Terminal => True,
      Tags => Alire_TOML.Read_Tags);

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
         Put_Line (Standard_Error,
                   "Downloading linuxdeploy failed. Please, install wget or curl.");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;
   end if;

   Runner.Run_Linuxdeploy (Executable => Alire_TOML.Read_Field ("name"),
                           Icon_File => Ada.Directories.Simple_Name (AP.String_Value ("icon")),
                           Success => Success);
   if not Success then
      Put_Line (Standard_Error, "Running linuxdeploy failed. Aborting.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   else
      Put_Line ("Success! AppImage is ready.");
   end if;

end Alr_Appimage;
