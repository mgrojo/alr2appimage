with Ada.Strings.Unbounded;
with String_Vectors;

package Deb_Package is

   -- Convert a crate name to a Debian package name following the rules in
   --   https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-source
   --
   function Debian_Name_Of (Name : String) return String;

   -- Convert an Alire architecture name to a Debian architecture name following the rules in
   --   https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-architecture
   --
   function Debian_Architecture_Of (Arch : String) return String;

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
