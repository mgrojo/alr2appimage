package Runner is

   -- Create a temporary directory, install the application on it, and
   -- return the created application directory.
   --
   function Run_Alr_Install (Icon : String) return String;

   type Getter_Tool is (Curl, Wget);

   -- Download the linuxdeploy tool using the tool passed as argument.
   -- Set Success to True when OK, adn to False when the operation fails.
   --
   procedure Run_Get_Linuxdeploy (Use_Tool : Getter_Tool;
                                  Success : out Boolean);

   -- Run the linuxdeploy tool with the passed parameters.
   -- Set Success to True when OK, adn to False when the operation fails.
   --
   procedure Run_Linuxdeploy (App_Dir, Executable, Icon_File : String;
                              Success : out Boolean);

end Runner;
