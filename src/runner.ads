with Ada.Strings.Unbounded;

package Runner is

   -- Create a temporary directory, install the application on it, and
   -- return the created application directory.
   --
   -- Install the Icon file and the Desktop_Filename if not empty.
   function Run_Alr_Install (Icon, Desktop_Filename : String) return String;

   type Getter_Tool is (Curl, Wget);

   -- Download the linuxdeploy tool using the tool passed as argument.
   -- Set Success to True when OK, adn to False when the operation fails.
   --
   procedure Run_Get_Linuxdeploy
     (Use_Tool : Getter_Tool; Success : out Boolean);

   -- Run the linuxdeploy tool with the passed parameters.
   -- If Version is empty, the version is not set for the AppImage.
   -- Set Success to True when OK, and to False when the operation fails.
   --
   procedure Run_Linuxdeploy
     (App_Dir, Executable, Icon_File, Version : String; Success : out Boolean);


   -- Set Success to True when OK, and to False when the operation fails.
   --
   procedure Run_Dpkg_Dependencies
     (App_Dir, Executable : String;
      Success : out Boolean;
      Dependencies : out Ada.Strings.Unbounded.Unbounded_String);

   -- Set Success to True when OK, and to False when the operation fails.
   --
   procedure Run_Dpkg_Build
     (App_Dir, Filename : String; Success : out Boolean);

end Runner;
