with Ada.Strings.Unbounded;
with String_Vectors;

package Deb_Package is

   -- Write a control file for a Debian package with the fields passed as parameters.
   -- See https://www.debian.org/doc/debian-policy/ch-controlfields.html
   --
   procedure Write_Control
     (App_Dir        : String;
      Name           : String;
      Comment        : String;
      Exec           : String;
      Maintainer     : String;
      Version        : String;
      Tags           : String_Vectors.Vector;
      Dependencies   : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
      Include_Source : Boolean := False);

end Deb_Package;
