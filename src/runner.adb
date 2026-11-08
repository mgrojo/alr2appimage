with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO;

with Deb_Package;
with File_Manager;

with Shell;

package body Runner is

   App_Dir_Template : constant String := "alr2appimage-AppDir-XXXXXX";

   Installation_Subdir : constant String := "/usr";
   Icon_Path           : constant String :=
     "/share/icons/hicolor/128x128/apps/";

   Linuxdeploy_Program : constant String :=
     File_Manager.To_AppImage_File ("linuxdeploy");
   Linuxdeploy_URL     : constant String :=
     "https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/"
     & Linuxdeploy_Program;

   function To_Argument (Value : String) return Shell.Unbounded_String is
   begin
      return Shell."+" (Value);
   end To_Argument;

   procedure Report_State (Process : in out Shell.Process; Success : out Boolean) is
      Process_State : constant Shell.Process_State := Shell.Status (Process);
   begin

      Success := False;

      case Process_State is
         when Shell.Normal_Exit =>
            Ada.Text_IO.Put_Line ("OK");
            Success := True;

         when Shell.Not_Started | Shell.Running | Shell.Paused |
           Shell.Failed_Exit =>
            Ada.Text_IO.Put_Line ("with status " & Process_State'Image);

         when Shell.Interrupted | Shell.Killed =>
            Ada.Text_IO.Put_Line ("with signal " & Process_State'Image);
      end case;

   end Report_State;

   procedure Run_Command
     (Executable : String;
      Arguments  : Shell.String_Array;
      Success    : out Boolean)
   is
      Process : Shell.Process := Shell.Start (Program => Executable,
                                              Arguments => Arguments);
   begin
      Shell.Wait_On (Process);
      Report_State (Process, Success);
   end Run_Command;

   procedure Run_Shell_Command
     (Command : String;
      Success : out Boolean)
   is
      Args : constant Shell.String_Array :=
         (1 => To_Argument ("-c"),
          2 => To_Argument (Command));
   begin
      Run_Command ("sh", Args, Success);
   end Run_Shell_Command;

   procedure Run_Change_Mode
     (Filename : String; Mode : String; Success : out Boolean)
   is
      Args : constant Shell.String_Array :=
        (1 => To_Argument (Mode), 2 => To_Argument (Filename));
   begin
      Run_Command ("chmod", Args, Success);
      Ada.Text_IO.Put_Line
        ("Finished: ""chmod " & Mode & " " & Filename & """");
   end Run_Change_Mode;

   procedure Run_Copy
     (Filename     : String;
      To_Dir       : String;
      New_Filename : String := "";
      Success      : out Boolean)
   is
      Target_Filename : constant String :=
        (if New_Filename = ""
         then Ada.Directories.Simple_Name (Filename)
         else New_Filename);
   begin

      Ada.Text_IO.Put_Line ("Copying " & Filename & " to " & To_Dir & "...");

      if not Ada.Directories.Exists (To_Dir) then
         Ada.Directories.Create_Path (To_Dir);
         Run_Change_Mode (To_Dir, "755", Success);
      end if;

      begin
         Ada.Directories.Copy_File
           (Source_Name => Filename,
            Target_Name => To_Dir & "/" & Target_Filename);
         Run_Change_Mode (To_Dir & "/" & Target_Filename, "644", Success);
      exception
         when others =>
            Success := False;
      end;


   end Run_Copy;

   --  Run gprinstall inside "alr exec". This is the fallback when running under
   --  a version of Alire without the "install" subcommand (1.x versions).
   --
   procedure Run_Gprinstall (Install_Prefix : String; Success : out Boolean) is
      Args : constant Shell.String_Array :=
        (1 => To_Argument ("exec"),
         2 => To_Argument ("-P"),
         3 => To_Argument ("--"),
         4 => To_Argument ("gprinstall"),
         5 => To_Argument ("--prefix=" & Install_Prefix),
         6 => To_Argument ("--create-missing-dirs"),
         7 => To_Argument ("--mode=usage"),
         8 => To_Argument ("-f"));
   begin

      Run_Command ("alr", Args, Success);


      Ada.Text_IO.Put_Line
        ("Finished: fallback command: ""alr exec ... gprinstall ...""");
   end Run_Gprinstall;

   function Run_Alr_Install
     (Name, Icon, Desktop_Filename : String;
      Package_Kind                 : Basic_Types.Package_Kind) return String
   is

      Temporary_Dir : constant String :=
        File_Manager.Create_Temporary_Directory (App_Dir_Template);

      App_Dir : constant String :=
        (if Package_Kind in Basic_Types.Deb | Basic_Types.Rpm
         then Temporary_Dir & "/AppDir"
         else Temporary_Dir);

      Install_Prefix : constant String := App_Dir & Installation_Subdir;

      -- AppImage installation requires the AppDir to be executable by all users.
      New_Mode_Args : constant Shell.String_Array :=
        (1 => To_Argument ("a+r,a+x"),
         2 => To_Argument (Temporary_Dir));

      Alr_Args : constant Shell.String_Array :=
        (1 => To_Argument ("--force"),
         2 => To_Argument ("install"),
         3 => To_Argument ("--prefix=" & Install_Prefix));

      Success : Boolean := False;
   begin

      if Package_Kind in Basic_Types.AppImage then
         Run_Change_Mode (Temporary_Dir, "a+r,a+x", Success);
         if not Success then
            Ada.Text_IO.Put_Line
              ("Failed to set permissions on " & Temporary_Dir);
            return "";
         end if;
      end if;

      Run_Shell_Command
        ("umask 022 && alr --force install --prefix=" & Install_Prefix,
         Success);
      Ada.Text_IO.Put_Line ("Finished: ""umask 022 && alr install""");

      if not Success then
         --  "alr install" failed. Let's try with the fallback command.
         Run_Gprinstall (Install_Prefix, Success);
      end if;

      if Success and then Icon /= "" then
         Run_Copy
           (Filename     => Icon,
            To_Dir       => Install_Prefix & Icon_Path,
            New_Filename => Name & "." & Ada.Directories.Extension (Icon),
            Success      => Success);
      end if;

      if Success and then Desktop_Filename /= "" then
         Run_Copy
           (Filename => Desktop_Filename,
            To_Dir   => Install_Prefix & "/share/applications/",
            Success  => Success);
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

   procedure Run_Get_Linuxdeploy
     (Use_Tool : Getter_Tool; Success : out Boolean) is
   begin

      if Ada.Directories.Exists (Linuxdeploy_Program) then
         Ada.Text_IO.Put_Line ("Reusing existing " & Linuxdeploy_Program);
         Success := True;
         return;
      end if;

      case Use_Tool is
         when Curl =>
            declare
               Args : constant Shell.String_Array :=
                 (1 => To_Argument ("--remote-name"),
                  2 => To_Argument ("--location"),
                  3 => To_Argument (Linuxdeploy_URL));
            begin
               Run_Command ("curl", Args, Success);
               Ada.Text_IO.Put_Line ("Finished: curl...");
            end;

         when Wget =>
            declare
               Args : constant Shell.String_Array :=
                 (1 => To_Argument ("-nv"),
                  2 => To_Argument ("-c"),
                  3 => To_Argument (Linuxdeploy_URL));
            begin
               Run_Command ("wget", Args, Success);
               Ada.Text_IO.Put_Line ("Finished: wget...");
            end;

      end case;

   end Run_Get_Linuxdeploy;

   -- Find the first file with Executable name in the current directory
   --
   function Find_Executable
     (Executable : String; From_Dir : String) return String
   is

      use Ada.Directories;

      File_Not_Found : exception;
      Found_Filename : Ada.Strings.Unbounded.Unbounded_String;

      procedure Find_First_File
        (Current_Dir : String; Deepness : in out Natural)
      is
         Maximum_Deepness : constant Natural := 5;
         Dir_Entry        : Directory_Entry_Type;
         Dir_Search       : Search_Type;

      begin
         if Deepness = Maximum_Deepness then
            return;
         end if;
         Deepness := Deepness + 1;
         Start_Search
           (Search    => Dir_Search,
            Directory => Current_Dir,
            Pattern   => "",
            Filter    =>
              (Ordinary_File => True, Directory => True, others => False));

         while More_Entries (Dir_Search) loop
            Get_Next_Entry (Dir_Search, Dir_Entry);

            case Kind (Dir_Entry) is
               when Directory                    =>
                  -- Recursive call avoiding the link to current directory (".")
                  if Simple_Name (Dir_Entry) /= "."
                    and then Simple_Name (Dir_Entry) /= ".."
                  then
                     Find_First_File
                       (Current_Dir => Full_Name (Dir_Entry),
                        Deepness    => Deepness);
                  end if;

               when Ordinary_File                =>
                  if Simple_Name (Dir_Entry) = Executable then
                     Found_Filename :=
                       Ada.Strings.Unbounded.To_Unbounded_String
                         (Full_Name (Dir_Entry));
                     exit;
                  end if;

               when Ada.Directories.Special_File =>
                  null;
            end case;

            exit when not More_Entries (Dir_Search);
         end loop;

         End_Search (Dir_Search);

         Deepness := Deepness - 1;

      end Find_First_File;

      Deepness : Natural := 0;
   begin

      Find_First_File (From_Dir, Deepness);

      if Ada.Strings.Unbounded.Length (Found_Filename) = 0 then
         raise File_Not_Found
           with Executable & " not found in crate directory.";
      end if;

      return Ada.Strings.Unbounded.To_String (Found_Filename);
   end Find_Executable;

   procedure Fix_Permissions (Directory : String; Success : out Boolean)
   is
   begin
      Run_Shell_Command
        ("find """ & Directory & """ -type d -exec chmod 755 {} +", Success);
   end Fix_Permissions;

   procedure Run_Linuxdeploy
     (App_Dir, Executable, Icon_File, Version : String; Success : out Boolean)
   is

      Install_Prefix : constant String := App_Dir & Installation_Subdir;

      Arg_String_1 : constant String :=
        "--executable="
        & Find_Executable
            (Executable, From_Dir => Ada.Directories.Current_Directory);
      Arg_String_2 : constant String :=
        "--desktop-file=" & Executable & ".desktop";
      Arg_String_3 : constant String :=
        "--icon-file=" & Install_Prefix & Icon_Path & Icon_File;
      Arg_String_4 : constant String := "--appdir=" & App_Dir;
      Arg_String_5 : constant String := "--output=appimage";

      Args : constant Shell.String_Array :=
        (1 => To_Argument (Arg_String_1),
         2 => To_Argument (Arg_String_2),
         3 => To_Argument (Arg_String_3),
         4 => To_Argument (Arg_String_4),
         5 => To_Argument (Arg_String_5));
   begin

      if Version /= "" then
         Ada.Environment_Variables.Set (Name => "VERSION", Value => Version);
      end if;

      Run_Change_Mode (Linuxdeploy_Program, "+x", Success);

      if not Success then
         return;
      end if;

      Ada.Text_IO.Put_Line
        ("Running: ""./"
         & Linuxdeploy_Program
         & " "
         & Arg_String_1
         & " "
         & Arg_String_2
         & " "
         & Arg_String_3
         & " "
         & Arg_String_4
         & " "
         & Arg_String_5
         & '"');

      declare
         Process : Shell.Process :=
           Shell.Start (Program => "./" & Linuxdeploy_Program,
                       Arguments => Args);
      begin
         Shell.Wait_On (Process);
         Report_State (Process, Success);
      end;

   end Run_Linuxdeploy;

   procedure Write_Deb_Copyright
     (App_Dir    : String;
      Name       : String;
      Source_URL : String;
      Author     : String;
      License    : String)
   is
      Success        : Boolean := False;
      Base_Path      : constant String := App_Dir & "/usr/share/doc/";
      Copyright_Path : constant String := Base_Path &
         Deb_Package.Debian_Name_Of (Name);
      Copyright_File : constant String := Copyright_Path & "/copyright";
   begin

      -- Let user define their own copyright file if they want to.
      -- If it exists, we don't overwrite it.
      if not Ada.Directories.Exists (Copyright_File) then

         Ada.Directories.Create_Path (Copyright_Path);

         Deb_Package.Write_Copyright
           (Filename   => Copyright_File,
            Name       => Name,
            Source_URL => Source_URL,
            Author     => Author,
            License    => License);
      end if;

      Fix_Permissions (Base_Path, Success);
      if not Success then
         Ada.Text_IO.Put_Line
           ("Warning: Failed to change mode of " & Base_Path);
      end if;

      Run_Change_Mode (Copyright_File, "644", Success);
      if not Success then
         Ada.Text_IO.Put_Line
           ("Warning: Failed to change mode of " & Copyright_File);
      end if;

   end Write_Deb_Copyright;

   procedure Run_Dpkg_Dependencies
     (App_Dir, Executable : String;
      Success             : out Boolean;
      Dependencies        : out Ada.Strings.Unbounded.Unbounded_String)
   is

      Original_Dir : constant String := Ada.Directories.Current_Directory;
      Working_Dir  : constant String :=
        Ada.Directories.Containing_Directory
          (Ada.Directories.Full_Name (App_Dir));

      Dependencies_Program : constant String := "dpkg-shlibdeps";
      Arg_String_1         : constant String := "-e";
      Arg_String_2         : constant String :=
        Ada.Directories.Simple_Name (App_Dir)
        & Installation_Subdir
        & "/bin/"
        & Executable;

      Args : constant Shell.String_Array :=
        (1 => To_Argument (Arg_String_1),
         2 => To_Argument (Arg_String_2));
   begin

      Success := False;
      Dependencies := Ada.Strings.Unbounded.Null_Unbounded_String;

      begin
         Ada.Directories.Set_Directory (Working_Dir);

         Ada.Text_IO.Put_Line
           ("Running: """
            & Dependencies_Program
            & " "
            & Arg_String_1
            & " "
            & Arg_String_2
            & '"'
            & " in directory: "
            & Working_Dir);

         declare
            Process           : Shell.Process :=
              Shell.Start (Program => Dependencies_Program,
                          Arguments => Args);
            Dependencies_File : Ada.Text_IO.File_Type;
            Unwanted_Prefix   : constant String := "shlibs:Depends=";
         begin
            Shell.Wait_On (Process);
            Report_State (Process, Success);

            if Success then
               --  Read the dependencies from the file "debian/substvars"
               --  and remove the prefix "shlibs:Depends=".
               --
               Ada.Text_IO.Open
                 (File => Dependencies_File,
                  Name => "debian/substvars",
                  Mode => Ada.Text_IO.In_File);

               Dependencies :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                   (Ada.Text_IO.Get_Line (File => Dependencies_File));
               Dependencies :=
                 Ada.Strings.Unbounded.Unbounded_Slice
                   (Dependencies,
                    Unwanted_Prefix'Length + 1,
                    Ada.Strings.Unbounded.Length (Dependencies));
               Ada.Text_IO.Close (Dependencies_File);
            end if;
         end;
      exception
         when others =>
            Success := False;
      end;

      Ada.Directories.Set_Directory (Original_Dir);
   end Run_Dpkg_Dependencies;

   procedure Run_Dpkg_Build (App_Dir, Filename : String; Success : out Boolean)
   is

      Dpkg_Program : constant String := "dpkg-deb";

      Arg_String_1 : constant String := "--root-owner-group";
      Arg_String_2 : constant String := "--build";
      Arg_String_3 : String renames App_Dir;
      Arg_String_4 : String renames Filename;

      Args : constant Shell.String_Array :=
        (1 => To_Argument (Arg_String_1),
         2 => To_Argument (Arg_String_2),
         3 => To_Argument (Arg_String_3),
         4 => To_Argument (Arg_String_4));
   begin

      Ada.Text_IO.Put_Line
        ("Running: """
         & Dpkg_Program
         & " "
         & Arg_String_1
         & " "
         & Arg_String_2
         & " "
         & Arg_String_3
         & " "
         & Arg_String_4
         & '"');

      declare
         Process : Shell.Process :=
           Shell.Start (Program => Dpkg_Program,
                       Arguments => Args);
      begin
         Shell.Wait_On (Process);
         Report_State (Process, Success);
      end;

   end Run_Dpkg_Build;

   procedure Run_Rpm_Build
     (App_Dir, Spec_File, Filename : String; Success : out Boolean)
   is
      Original_Dir : constant String := Ada.Directories.Current_Directory;
      Working_Dir  : constant String :=
        Ada.Directories.Containing_Directory
          (Ada.Directories.Full_Name (App_Dir));
      --  Keep RPM build sources outside %{buildroot}. rpmbuild cleans
      --  buildroot before %install, so using App_Dir as both source and
      --  destination would remove the files to package.
      Build_Dir    : constant String := Working_Dir & "/BUILD";

      Rpmbuild_Program : constant String := "rpmbuild";

      Arg_String_1  : constant String := "--buildroot";
      Arg_String_2  : constant String := Ada.Directories.Full_Name (App_Dir);
      Arg_String_3  : constant String := "-bb";
      Arg_String_4  : constant String :=
        "SPECS/" & Ada.Directories.Simple_Name (Spec_File);
      Arg_String_5  : constant String := "--define";
      Arg_String_6  : constant String := "_rpmdir " & Original_Dir;
      Arg_String_7  : constant String := "--define";
      Arg_String_8  : constant String := "_build_name_fmt " & Filename;
      Arg_String_9  : constant String := "--define";
      --  Point rpmbuild to the staged tree where %install can read AppDir/*.
      Arg_String_10 : constant String := "_builddir " & Build_Dir;
      Arg_String_11 : constant String := "--define";
      Arg_String_12 : constant String := "_unpackaged_files_terminate_build 0";

      Args : constant Shell.String_Array :=
        (1 => To_Argument (Arg_String_1),
         2 => To_Argument (Arg_String_2),
         3 => To_Argument (Arg_String_3),
         4 => To_Argument (Arg_String_4),
         5 => To_Argument (Arg_String_5),
         6 => To_Argument (Arg_String_6),
         7 => To_Argument (Arg_String_7),
         8 => To_Argument (Arg_String_8),
         9 => To_Argument (Arg_String_9),
         10 => To_Argument (Arg_String_10),
         11 => To_Argument (Arg_String_11),
         12 => To_Argument (Arg_String_12));
   begin
      begin
         Ada.Directories.Create_Path (Build_Dir);
         Run_Change_Mode (Build_Dir, "755", Success);

         if not Success then
            Ada.Text_IO.Put_Line
              ("Failed to change permissions of directory " & Build_Dir);
            return;
         end if;

         declare
            --  Stage App_Dir under Build_Dir so the spec's "%install"
            --  can copy from AppDir/* into %{buildroot}.
            Copy_Args : constant Shell.String_Array :=
              (1 => To_Argument ("-a"),
               2 => To_Argument (App_Dir),
               3 => To_Argument (Build_Dir));
            Copy_Process : Shell.Process :=
              Shell.Start (Program => "cp",
                           Arguments => Copy_Args);
         begin
            Shell.Wait_On (Copy_Process);
            Report_State (Copy_Process, Success);

            if not Success then
               return;
            end if;
         end;

         Ada.Directories.Set_Directory (Working_Dir);

         Ada.Text_IO.Put_Line
           ("Running: "
            & Rpmbuild_Program
            & " "
            & Arg_String_1
            & " "
            & Arg_String_2
            & " "
            & Arg_String_3
            & " "
            & Arg_String_4
            & " "
            & Arg_String_5
            & " "
            & Arg_String_6
            & " "
            & Arg_String_7
            & " "
            & Arg_String_8
            & " "
            & Arg_String_9
            & " "
            & Arg_String_10
            & " "
            & Arg_String_11
            & " "
            & Arg_String_12
            & " in directory: "
            & Working_Dir);

         declare
            Process : Shell.Process :=
              Shell.Start (Program => Rpmbuild_Program,
                          Arguments => Args);
         begin
            Shell.Wait_On (Process);
            Report_State (Process, Success);
         end;
      exception
         when others =>
            Success := False;
      end;

      Ada.Directories.Set_Directory (Original_Dir);
   end Run_Rpm_Build;

end Runner;
