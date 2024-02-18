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
