package File_Manager is

   Creation_Error : exception;

   -- Bind to POSIX mkdtemp(3) to create a temporary directory.
   -- Raises Creation_Error when the call fails.
   --
   function Create_Temporary_Directory (Template : String) return String;

end File_Manager;
