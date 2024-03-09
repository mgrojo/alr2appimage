with Ada.Text_IO;

with Spoon;

package body Runner is

   App_Dir : constant String := "AppDir";
   Install_Prefix : constant String := App_Dir & "/usr";

   Linuxdeploy_Program : constant String := "linuxdeploy-x86_64.AppImage";
   Linuxdeploy_URL : constant String :=
     "https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/"
     & Linuxdeploy_Program;

   Linuxdeploy_URL_Argument : aliased Spoon.Argument
     := Spoon.To_Argument (Linuxdeploy_URL);

   ---------------------
   -- Run_Alr_Install --
   ---------------------

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

   procedure Run_Copy_Icon (Icon : String; Success : out Boolean) is

      Arg_1 : aliased Spoon.Argument := Spoon.To_Argument ("-p");
      Arg_2 : aliased Spoon.Argument := Spoon.To_Argument (Icon);
      Arg_3 : aliased Spoon.Argument := Spoon.To_Argument (Install_Prefix);

      Result : constant Spoon.Result :=
        Spoon.Spawn (Executable => "cp",
                     Arguments => (Arg_1'Unchecked_Access,
                                   Arg_2'Unchecked_Access,
                                   Arg_3'Unchecked_Access),
                     Kind => Spoon.Name);
   begin

      Ada.Text_IO.Put_Line ("Copying icon...");

      Report_State (Result, Success);

   end Run_Copy_Icon;

   procedure Run_Alr_Install (Icon : String; Success : out Boolean) is

      Arg_1 : aliased Spoon.Argument := Spoon.To_Argument ("--force");
      Arg_2 : aliased Spoon.Argument := Spoon.To_Argument ("install");
      Arg_3 : aliased Spoon.Argument
        := Spoon.To_Argument ("--prefix=" & Install_Prefix);

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
         Run_Copy_Icon (Icon, Success);
      end if;

   end Run_Alr_Install;

   -------------------------
   -- Run_Get_Linuxdeploy --
   -------------------------

   procedure Run_Get_Linuxdeploy (Use_Tool : Getter_Tool;
                                  Success : out Boolean)
   is
   begin

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

   ---------------------
   -- Run_Linuxdeploy --
   ---------------------

   procedure Run_Linuxdeploy (Executable, Icon_File : String; Success : out Boolean) is

      Arg_String_1 : constant String :="--executable=bin/" & Executable;
      Arg_String_2 : constant String :="--desktop-file=" & Executable & ".desktop";
      Arg_String_3 : constant String :="--icon-file=" & Install_Prefix & "/" & Icon_File;
      Arg_String_4 : constant String :="--appdir=" & App_Dir;
      Arg_String_5 : constant String :="--output=appimage";

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
