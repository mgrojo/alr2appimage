with Ada.Strings.Unbounded;

with String_Vectors;

package Desktop_File is

   function "+" (Item : String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   type String_Array is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Write
     (Name : String;
      Comment : String;
      Exec : String;
      Icon : String;
      Terminal : Boolean;
      Tags : String_Vectors.Vector);

end Desktop_File;
