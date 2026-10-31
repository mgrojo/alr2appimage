with Basic_Types;
with String_Vectors;

package File_Manager is

   Creation_Error : exception;

   -- Bind to POSIX mkdtemp(3) to create a temporary directory.
   -- Raises Creation_Error when the call fails.
   --
   function Create_Temporary_Directory (Template : String) return String;

   function To_Package_File
     (Package_Kind : Basic_Types.Package_Kind;
      Name         : String;
      Version      : String := "") return String;

   function To_AppImage_File
     (Name         : String;
      Version      : String := "") return String
   is (To_Package_File (Basic_Types.AppImage, Name, Version));

   -- Walk a Directory tree and return the full path of each file found in Files.
   procedure Walk_Directory
     (Directory : in String;
      Files     : in out String_Vectors.Vector);

end File_Manager;
