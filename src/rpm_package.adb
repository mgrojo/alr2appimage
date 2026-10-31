with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Alr2appimage_Config;
with File_Manager;
with String_Vectors;

package body Rpm_Package is

   function Rpm_Architecture_Of (Arch : String) return String is
   begin
      if Arch = "i386" or else Arch = "i686" then
         return "i686";
      else
         return Arch;
      end if;
   end Rpm_Architecture_Of;

   function Rpm_Version_Of (Version : String) return String is
      Dash : constant Natural := Ada.Strings.Fixed.Index (Version, "-");
   begin
      if Dash = 0 then
         return Version;
      elsif Dash = Version'First then
         return Version;
      else
         return Version (Version'First .. Dash - 1);
      end if;
   end Rpm_Version_Of;

   function Rpm_Release_Of (Version : String) return String is
      Dash : constant Natural := Ada.Strings.Fixed.Index (Version, "-");
   begin
      if Dash = 0 then
         return "1";
      elsif Dash < Version'Last then
         return Version (Dash + 1 .. Version'Last);
      else
         return "1";
      end if;
   end Rpm_Release_Of;

   Field_Width : constant Natural := 11;
   function Put_Field (Key, Value : String) return String
   is (Key & ":" & (1 .. Field_Width - Key'Length => ' ') & Value);

   function Write_Spec
     (App_Dir  : String;
      Name     : String;
      Version  : String;
      Summary  : String;
      License  : String) return String
   is
      Working_Dir : constant String :=
        Ada.Directories.Containing_Directory
          (Ada.Directories.Full_Name (App_Dir));
      Specs_Dir : constant String := Working_Dir & "/SPECS";
      Spec_Path : constant String := Specs_Dir & "/" & Name & ".spec";

      Spec_File : File_Type;
   begin
      Ada.Directories.Create_Path (Specs_Dir);

      Create (File => Spec_File, Name => Spec_Path);

      Put_Line (Spec_File, Put_Field ("Name", Name));
      Put_Line (Spec_File, Put_Field ("Version", Rpm_Version_Of (Version)));
      Put_Line (Spec_File, Put_Field ("Release", Rpm_Release_Of (Version)));
      Put_Line (Spec_File, Put_Field ("Summary", Summary));
      New_Line (Spec_File);
      Put_Line (Spec_File, Put_Field ("License", License));
      Put_Line
        (Spec_File,
         Put_Field ("BuildArch", Rpm_Architecture_Of (Alr2appimage_Config.Alire_Host_Arch)));
      New_Line (Spec_File);
      Put_Line (Spec_File, "AutoReqProv: yes");
      New_Line (Spec_File);

      Put_Line (Spec_File, "%description");
      Put_Line (Spec_File, Summary);
      New_Line (Spec_File);

      Put_Line (Spec_File, "%install");
      Put_Line (Spec_File, "mkdir -p %{buildroot}");
      Put_Line (Spec_File, "cp -a AppDir/* %{buildroot}/");
      New_Line (Spec_File);

      Put_Line (Spec_File, "%files");
      declare
         Files : String_Vectors.Vector;
      begin
         File_Manager.Walk_Directory (App_Dir, Files);
         for File of Files loop
            Put_Line (Spec_File, "/" &
               File (App_Dir'Length + 2 .. File'Last));
         end loop;
      end;

      Close (Spec_File);

      return Spec_Path;
   end Write_Spec;

end Rpm_Package;
