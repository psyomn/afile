with Interfaces; use Interfaces;
with Headers; use Headers;

package Identify is
   function Try (H : Unsigned_64) return Boolean;
   procedure Identify_File (Filename : String);
end Identify;
