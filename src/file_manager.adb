with Interfaces.C.Strings;

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

end File_Manager;
