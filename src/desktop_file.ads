with String_Vectors;

package Desktop_File is

   -- Write a desktop entry file with the fields passed as parameters.
   -- The file will be named: Name & ".desktop".
   -- See https://specifications.freedesktop.org/desktop-entry-spec/latest/
   --
   procedure Write
     (Name : String;
      Comment : String;
      Exec : String;
      Icon : String;
      Terminal : Boolean;
      Tags : String_Vectors.Vector);

end Desktop_File;
