with String_Vectors;

package Alire_TOML is

   procedure Load_Alire;

   function Read_Field (Key : String; Default : String := "") return String;

   function Read_String_List (Key : String) return String_Vectors.Vector;

end Alire_TOML;
