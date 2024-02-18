package Runner is

   procedure Run_Alr_Install (Icon : String; Success : out Boolean);

   type Getter_Tool is (Curl, Wget);

   procedure Run_Get_Linuxdeploy (Use_Tool : Getter_Tool;
                                  Success : out Boolean);

   procedure Run_Linuxdeploy (Executable, Icon_File : String; Success : out Boolean);

end Runner;
