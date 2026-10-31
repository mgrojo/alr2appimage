with Ada.Directories;
with Alr2appimage_Config;
with Deb_Package;
with Interfaces.C.Strings;
with Rpm_Package;

package body File_Manager is

   -- Import of POSIX mkdtemp(3) to create a temporary directory.
   --
   function mkdtemp (template : in out Interfaces.C.char_array)
                    return Interfaces.C.Strings.chars_ptr
     with Import, Convention => C, External_Name => "mkdtemp";


   function Create_Temporary_Directory (Template : String) return String
   is
      C_Template : Interfaces.C.char_array := Interfaces.C.To_C (Template);
      C_Result : constant Interfaces.C.Strings.chars_ptr := mkdtemp (C_Template);
   begin

      if Interfaces.C.Strings."=" (C_Result, Interfaces.C.Strings.Null_Ptr) then
         raise Creation_Error;
      end if;

      return Interfaces.C.Strings.Value (C_Result);
   end Create_Temporary_Directory;

   function To_Package_File
     (Package_Kind : Basic_Types.Package_Kind;
      Name         : String;
      Version      : String := "") return String
   is
      Version_Part : constant String :=
        (if Version = "" then "" else Version & '-');
   begin

      case Package_Kind is
         when Basic_Types.AppImage =>
            return
              Name
              & '-'
              & Version_Part
              & Alr2appimage_Config.Alire_Host_Arch
              & ".AppImage";

         when Basic_Types.Deb =>
            return
              Deb_Package.Debian_Name_Of (Name)
              & '-'
              & Version_Part
              & Deb_Package.Debian_Architecture_Of
                  (Alr2appimage_Config.Alire_Host_Arch)
              & ".deb";

         when Basic_Types.Rpm =>
            return
              Name
              & '-'
              & Rpm_Package.Rpm_Version_Of (Version)
              & '-'
              & Rpm_Package.Rpm_Release_Of (Version)
              & '.'
              & Rpm_Package.Rpm_Architecture_Of
                  (Alr2appimage_Config.Alire_Host_Arch)
              & ".rpm";

      end case;
   end To_Package_File;

   procedure Walk_Directory
      (Directory : in String;
       Files     : in out String_Vectors.Vector)
   is
      use type Ada.Directories.File_Kind;
      Pattern : constant String := "";
      Search  : Ada.Directories.Search_Type;
      Dir_Ent : Ada.Directories.Directory_Entry_Type;
   begin
      Ada.Directories.Start_Search (Search, Directory, Pattern);

      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Dir_Ent);
         if Ada.Directories.Kind (Dir_Ent) = Ada.Directories.Directory then
            if Ada.Directories.Simple_Name (Dir_Ent) /= "." and then
               Ada.Directories.Simple_Name (Dir_Ent) /= ".."
            then
               Walk_Directory (Ada.Directories.Full_Name (Dir_Ent), Files);
            end if;
         else
            Files.Append (Ada.Directories.Full_Name (Dir_Ent));
         end if;
      end loop;

      Ada.Directories.End_Search (Search);
   end Walk_Directory;

end File_Manager;
