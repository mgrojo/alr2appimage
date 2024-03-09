with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with TOML.File_IO;

package body Alire_TOML is

   use TOML;

   Filename : constant String := "alire.toml";

   Config : TOML_Value;

   function Read_Field (Key : String; Default : String := "") return String is
   begin
      if Config /= No_TOML_Value and then
        Config.Has (Key => Key)
      then

         return Config.Get (Key).As_String;
      else
         return Default;
      end if;

   exception
      when others =>

         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Error: invalid format for " &
                                Key & " in " & Filename);
         return Default;
   end Read_Field;

   function Read_String_List
     (Key :  String) return String_Vectors.Vector is

      Default : String_Vectors.Vector renames String_Vectors.Empty_Vector;
      Value : String_Vectors.Vector := Default;
   begin

      if Config /= No_TOML_Value and then
        Config.Has (Key => Key)
      then
         declare
            TOML_Tags : TOML.TOML_Value renames Config.Get (Key);
         begin
            for Index in 1 .. TOML_Tags.Length loop
               String_Vectors.Container.Append (Value, TOML_Tags.Item (Index).As_String);
            end loop;
         end;
      end if;

      return Value;

   exception
      when others =>

         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Error: invalid format for " &
                                 Key & " in " & Filename);
         return Default;

   end Read_String_List;

   procedure Load_Alire is
   begin

      if not Ada.Directories.Exists (Filename) then

         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Error: not inside an Alire workspace (alire.toml not found)");

         return;
      end if;

      declare
         Result : constant TOML.Read_Result :=
           TOML.File_IO.Load_File (Filename);
      begin
         if Result.Success then
            Config := Result.Value;
         else
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                             "Error: while loading " & Filename & ":" &
                               Result.Location.Line'Image & ":" &
                               Result.Location.Column'Image & ": ");
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Ada.Strings.Unbounded.To_String (Result.Message));

            return;
         end if;
      end;

   end Load_Alire;

end Alire_TOML;
