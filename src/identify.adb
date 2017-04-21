with Ada.Text_Io; use Ada.Text_Io;
with Ada.Streams.Stream_Io; use Ada.Streams.Stream_Io;
with Interfaces; use Interfaces;

with Headers;

package body Identify is
   function Try (H : Unsigned_64) return Boolean is
   begin
      for I in Integer range Headers.All_File_Signatures'Range loop
         declare
            Curr_Sig : constant Headers.File_Signature :=
              Headers.All_File_Signatures (I);

            Treated_Header : constant Unsigned_64 :=
              (if Curr_Sig.Bits = 0 then
                 H
               else
                 Shift_Right (H, Curr_Sig.Bits));
         begin
            if Curr_Sig.Magic_Number = Treated_Header then
               Headers.Print_File_Info (Curr_Sig);
               return True;
            end if;
         end;
      end loop;
      return False;
   end Try;

   procedure Identify_File
     (Filename     : String) is
      Input_File   : Ada.Streams.Stream_Io.File_Type;
      Input_Stream : Ada.StrEams.Stream_Io.Stream_Access;
      Num_Bytes    : Natural                := 8;
      Element      : Interfaces.Unsigned_64 := 0;
      Group_Size   : Integer                := 0;
      U8           : Interfaces.Unsigned_8  := 0;
   begin

      Ada.Streams.Stream_Io.Open (
         Input_File,
         Ada.Streams.Stream_Io.In_File,
         Filename
      );

      Input_Stream := Ada.Streams.Stream_Io.Stream (Input_File);

      Get_Headers :
      while not Ada.Streams.Stream_Io.End_Of_File (Input_File) loop
         Interfaces.Unsigned_8'Read (Input_Stream, U8);

         Element := Shift_Left (Element, 8);
         Element := Element or Interfaces.Unsigned_64 (U8);

         Num_Bytes := Num_Bytes - 1;
         exit when Num_Bytes = 0;
      end loop Get_Headers;

      Ada.Streams.Stream_Io.Close(Input_File);

      Put (Filename & ": ");
      if Try (Element) then
         return;
      end if;

      raise Headers.Unknown_Header with "file could not be identified";
   end Identify_File;
end Identify;
