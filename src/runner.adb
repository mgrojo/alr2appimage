with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with File_Manager;

with Spoon;

package body Runner is

   App_Dir_Template : constant String := "alr-appimage-AppDir-XXXXXX";

   Installation_Subdir : constant String := "/usr";

   Linuxdeploy_Program : constant String := File_Manager.To_AppImage_File ("linuxdeploy");
   Linuxdeploy_URL : constant String :=
     "https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/"
     & Linuxdeploy_Program;

   Linuxdeploy_URL_Argument : aliased Spoon.Argument
     := Spoon.To_Argument (Linuxdeploy_URL);

   procedure Report_State (Result : Spoon.Result; Success : out Boolean) is
      use all type Spoon.Exit_State;
      use type Spoon.Exit_Status;
   begin

      Success := False;

      case Result.State is
         when Exited =>
            if Result.Exit_Status = Spoon.Success then
               Ada.Text_IO.Put_Line ("OK");
               Success := True;
            else
               Ada.Text_IO.Put_Line ("with status" & Result.Exit_Status'Image);
            end if;
         when Crashed | Terminated =>
            Ada.Text_IO.Put_Line ("with signal" & Result.Signal'Image);
         when Error =>
            Ada.Text_IO.Put_Line ("with error" & Result.Error_Code'Image);
      end case;

   end Report_State;

   procedure Run_Copy_Icon (Icon : String; To_Dir : String; Success : out Boolean) is

      Arg_1 : aliased Spoon.Argument := Spoon.To_Argument ("-p");
      Arg_2 : aliased Spoon.Argument := Spoon.To_Argument (Icon);
      Arg_3 : aliased Spoon.Argument := Spoon.To_Argument (To_Dir);

      Result : constant Spoon.Result :=
        Spoon.Spawn (Executable => "cp",
                     Arguments => (Arg_1'Unchecked_Access,
                                   Arg_2'Unchecked_Access,
                                   Arg_3'Unchecked_Access),
                     Kind => Spoon.Name);
   begin

      Ada.Text_IO.Put_Line ("Copying icon " & Icon & " to " & To_Dir & "...");

      Report_State (Result, Success);

   end Run_Copy_Icon;

   function Run_Alr_Install (Icon : String) return String
   is

      App_Dir : constant String := File_Manager.Create_Temporary_Directory (App_Dir_Template);

      Install_Prefix : constant String := App_Dir & Installation_Subdir;

      Arg_1 : aliased Spoon.Argument := Spoon.To_Argument ("--force");
      Arg_2 : aliased Spoon.Argument := Spoon.To_Argument ("install");
      Arg_3 : aliased Spoon.Argument
        := Spoon.To_Argument ("--prefix=" & Install_Prefix);

      Success : Boolean := False;

      Result : constant Spoon.Result :=
        Spoon.Spawn (Executable => "alr",
                     Arguments => (Arg_1'Unchecked_Access,
                                   Arg_2'Unchecked_Access,
                                   Arg_3'Unchecked_Access),
                     Kind => Spoon.Name);
   begin

      Ada.Text_IO.Put_Line ("Running ""alr install""");

      Report_State (Result, Success);

      if Success then
         Run_Copy_Icon (Icon, Install_Prefix, Success);
      end if;

      if not Success then
         return "";
      else
         return App_Dir;
      end if;

   end Run_Alr_Install;

   -------------------------
   -- Run_Get_Linuxdeploy --
   -------------------------

   procedure Run_Get_Linuxdeploy (Use_Tool : Getter_Tool;
                                  Success : out Boolean)
   is
   begin

      if Ada.Directories.Exists (Linuxdeploy_Program) then
         Ada.Text_IO.Put_Line ("Reusing existing " & Linuxdeploy_Program);
         Success := True;
         return;
      end if;

      case Use_Tool is
         when Curl =>
            declare
               Arg_1 : aliased Spoon.Argument
                 := Spoon.To_Argument ("--remote-name");
               Arg_2 : aliased Spoon.Argument
                 := Spoon.To_Argument ("--location");
               Result : constant Spoon.Result :=
                 Spoon.Spawn (Executable => "curl",
                              Arguments => (Arg_1'Unchecked_Access,
                                            Arg_2'Unchecked_Access,
                                            Linuxdeploy_URL_Argument'Access),
                              Kind => Spoon.Name);
            begin
               Ada.Text_IO.Put_Line ("Running curl...");
               Report_State (Result, Success);
            end;

         when Wget =>

            declare
               Arg_1 : aliased Spoon.Argument := Spoon.To_Argument ("-nv");
               Arg_2 : aliased Spoon.Argument := Spoon.To_Argument ("-c");

               Result : constant Spoon.Result :=
                 Spoon.Spawn (Executable => "wget",
                              Arguments => (Arg_1'Unchecked_Access,
                                            Arg_2'Unchecked_Access,
                                            Linuxdeploy_URL_Argument'Access),
                              Kind => Spoon.Name);
            begin

               Ada.Text_IO.Put_Line ("Running wget...");
               Report_State (Result, Success);
            end;

      end case;

   end Run_Get_Linuxdeploy;

   procedure Run_Change_Mode (Success : out Boolean) is

      Arg_1 : aliased Spoon.Argument := Spoon.To_Argument ("+x");
      Arg_2 : aliased Spoon.Argument
        := Spoon.To_Argument (Linuxdeploy_Program);

      Result : constant Spoon.Result :=
        Spoon.Spawn (Executable => "chmod",
                     Arguments => (Arg_1'Unchecked_Access,
                                   Arg_2'Unchecked_Access),
                     Kind => Spoon.Name);
   begin
      Ada.Text_IO.Put_Line ("Running ""chmod +x "
                              & Linuxdeploy_Program & """");
      Report_State (Result, Success);
   end Run_Change_Mode;

   -- Find the first file with Executable name in the current directory
   --
   function Find_Executable (Executable : String; From_Dir : String) return String
   is

      use Ada.Directories;

      File_Not_Found : exception;
      Found_Filename : Ada.Strings.Unbounded.Unbounded_String;

      procedure Find_First_File (Current_Dir : String) is

         Dir_Entry : Directory_Entry_Type;
         Dir_Search : Search_Type;

      begin
         Start_Search (Search => Dir_Search,
                       Directory => Current_Dir,
                       Pattern => "",
                       Filter => (Ordinary_File => True,
                                  Directory     => True,
                                  others        => False));

         while More_Entries (Dir_Search) loop
            Get_Next_Entry (Dir_Search, Dir_Entry);

            case Kind (Dir_Entry) is
               when Directory =>
                  -- Recursive call avoiding the link to current directory (".")
                  if Simple_Name (Dir_Entry) /= "."
                    and then
                    Simple_Name (Dir_Entry) /= ".."
                  then
                     Find_First_File
                       (Current_Dir => Full_Name (Dir_Entry));
                  end if;
               when Ordinary_File =>
                  if Simple_Name (Dir_Entry) = Executable then
                     Found_Filename :=
                       Ada.Strings.Unbounded.To_Unbounded_String (Full_Name (Dir_Entry));
                     exit;
                  end if;
               when Ada.Directories.Special_File =>
                  null;
            end case;

            exit when not More_Entries (Dir_Search);
         end loop;

         End_Search (Dir_Search);

      end Find_First_File;

   begin

      Find_First_File (From_Dir);

      if Ada.Strings.Unbounded.Length (Found_Filename) = 0 then
         raise File_Not_Found with Executable & " not found in crate directory.";
      end if;

      return Ada.Strings.Unbounded.To_String (Found_Filename);
   end Find_Executable;


   procedure Run_Linuxdeploy (App_Dir, Executable, Icon_File : String;
                              Success : out Boolean) is

      Install_Prefix : constant String := App_Dir & Installation_Subdir;

      Arg_String_1 : constant String := "--executable="
        & Find_Executable (Executable, From_Dir => Ada.Directories.Current_Directory);
      Arg_String_2 : constant String := "--desktop-file="
        & Executable & ".desktop";
      Arg_String_3 : constant String := "--icon-file="
        & Install_Prefix & "/" & Icon_File;
      Arg_String_4 : constant String := "--appdir="
        & App_Dir;
      Arg_String_5 : constant String := "--output=appimage";

      Arg_1 : aliased Spoon.Argument := Spoon.To_Argument (Arg_String_1);
      Arg_2 : aliased Spoon.Argument := Spoon.To_Argument (Arg_String_2);
      Arg_3 : aliased Spoon.Argument := Spoon.To_Argument (Arg_String_3);
      Arg_4 : aliased Spoon.Argument := Spoon.To_Argument (Arg_String_4);
      Arg_5 : aliased Spoon.Argument := Spoon.To_Argument (Arg_String_5);

   begin

      Run_Change_Mode (Success);

      if not Success then
         return;
      end if;

      Ada.Text_IO.Put_Line ("Running ""./" & Linuxdeploy_Program & " "
                              & Arg_String_1 & " "
                              & Arg_String_2 & " "
                              & Arg_String_3 & " "
                              & Arg_String_4 & " "
                              & Arg_String_5 & '"');

      declare
         Result : constant Spoon.Result :=
           Spoon.Spawn (Executable => "./" & Linuxdeploy_Program,
                        Arguments => (Arg_1'Unchecked_Access,
                                      Arg_2'Unchecked_Access,
                                      Arg_3'Unchecked_Access,
                                      Arg_4'Unchecked_Access,
                                      Arg_5'Unchecked_Access),
                        Kind => Spoon.File_Path);
      begin
         Report_State (Result, Success);
      end;

   end Run_Linuxdeploy;

end Runner;
