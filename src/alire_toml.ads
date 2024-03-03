with String_Vectors;

package Alire_TOML is

   function Read_Field (Key : String; Default : String := "") return String;

   function Read_Tags return String_Vectors.Vector;

   procedure Load_Alire;

end Alire_TOML;
