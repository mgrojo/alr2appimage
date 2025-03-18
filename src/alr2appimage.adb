with Ada.Command_Line;
with Ada.Containers;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Alire_TOML;
with Alr2appimage_Config;
with Desktop_File;
with File_Manager;
with Runner;
with String_Vectors;

with Parse_Args;
with Resources;

procedure Alr2AppImage is

   use Parse_Args;

   Abort_Error : exception;

   procedure Report_Failure (Message : String) is
   begin
      Put_Line (File => Standard_Error, Item => Message);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Report_Failure;

   package My_Resources is new Resources (Alr2appimage_Config.Crate_Name);

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

   AP.Add_Option (Make_String_Option (My_Resources.Resource_Path & "alr2appimage.png"),
                  "icon", 'i',
                  Usage => "Specify the icon file for the AppImage");

   AP.Add_Option (Make_String_Option (""),
                  "executable", 'e',
                  Usage => "Specify the executable for the AppImage (without path)");

   AP.Add_Option (Make_Boolean_Option (False),
                  "use-version", 'u',
                  Usage => "Use the crate version specified in the alire.toml for the AppImage");

   AP.Set_Prologue ("Makes an AppImage from your Alire crate.");

   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value ("help") then
      AP.Usage;
      return;

   elsif AP.Parse_Success and then AP.Boolean_Value ("version") then
      Put_Line (Alr2appimage_Config.Crate_Name & " version: " & Alr2appimage_Config.Crate_Version);
      return;

   elsif not AP.Parse_Success then
      Report_Failure ("Error while parsing command-line arguments: " & AP.Parse_Message);
      return;
   end if;

   Alire_TOML.Load_Alire;

   Make_AppImage : declare
      Executable_List : constant String_Vectors.Vector
        := Alire_TOML.Read_String_List ("executables");
      Icon : constant String := AP.String_Value ("icon");
      Name : constant String := Alire_TOML.Read_Field ("name");
      Version : constant String := (if AP.Boolean_Value ("use-version")
                                    then Alire_TOML.Read_Field ("version")
                                     else "");

      function Get_Executable return String
      is
      begin
         if AP.String_Value ("executable") /= "" then
            return AP.String_Value ("executable");
         elsif Ada.Containers."=" (Executable_List.Length, 0) then
            Report_Failure ("Error: the field 'executables' in 'alire.toml' is empty"
                              & " and no executable was provided on the command line.");
            raise Abort_Error;
         else
            return Executable_List.Element (Positive'First);
         end if;
      end Get_Executable;

      Executable : constant String := Get_Executable;
   begin

      Desktop_File.Write
        (Name => Name,
         Comment => Alire_TOML.Read_Field ("description"),
         Exec => Executable,
         Icon => Ada.Directories.Base_Name (Icon),
         Terminal => AP.Boolean_Value ("terminal"),
         Tags => Alire_TOML.Read_String_List ("tags"));

      Deploy : declare
         App_Dir : constant String := Runner.Run_Alr_Install (Icon);
      begin

         if App_Dir = "" then
            Report_Failure ("Running ""alr install"" Failed. Aborting.");
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

         Runner.Run_Linuxdeploy (App_Dir => App_Dir,
                                 Executable => Executable,
                                 Icon_File => Ada.Directories.Simple_Name (Icon),
                                 Version => Version,
                                 Success => Success);

         Ada.Directories.Delete_Tree (App_Dir);

         if not Success then
            Report_Failure ("Running linuxdeploy failed. Aborting.");
            return;
         else
            declare
               AppImage_File : constant String :=
                  File_Manager.To_AppImage_File (Name, Version);
            begin
               if Ada.Directories.Exists (AppImage_File) then
                  Put_Line ("Success! AppImage is ready in: " & AppImage_File);
               else
                  Report_Failure ("Error: " & AppImage_File &
                                     " file not found. Aborting.");
               end if;
            end;
         end if;

      end Deploy;
   end Make_AppImage;

end Alr2AppImage;
