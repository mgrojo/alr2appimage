with Alr2appimage_Config;

package File_Manager is

   Creation_Error : exception;

   -- Bind to POSIX mkdtemp(3) to create a temporary directory.
   -- Raises Creation_Error when the call fails.
   --
   function Create_Temporary_Directory (Template : String) return String;

   function To_AppImage_File (Name : String; Version : String := "") return String
   is (Name
       & '-'
       & (if Version = "" then "" else Version & '-')
       & Alr2appimage_Config.Alire_Host_Arch
       & ".AppImage");

end File_Manager;
