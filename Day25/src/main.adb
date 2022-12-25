-- Advent of Code 2022, Day 25
--
-- John Perry
--
-- Full of Hot Air
--
-- part 1: Figure out Bob's SNAFU number
--
-- part 2: freebee, as usual!
--

pragma Ada_2022;

with Ada.Text_IO;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   Bad_Snafu: exception;

   function SNAFU_To_Decimal(S: String) return Long_Long_Integer is
   -- converts `S` from a SNAFU string to a decimal number
      Result: Long_Long_Integer := 0;
   begin
      for I in S'First .. S'Last loop
         Result := Result * 5;
         case S(I) is
            when '0' | '1' | '2' =>
               Result := Result + Character'Pos(S(I)) - Character'Pos('0');
            when '-' =>
               Result := Result - 1;
            when '=' =>
               Result := Result - 2;
            when others =>
               raise Bad_Snafu;
         end case;
      end loop;
      return Result;
   end SNAFU_To_Decimal;

   type Fifty_Base5s is array(1..50) of Long_Long_Integer;
   -- used to store an array of numbers from -2 to 5

   procedure Romanize(B5: in out Fifty_Base5s) is
   -- "romanizes" `B5`, by which I mean that it turns `B5`,
   -- assumed to be a base-5 representation of a number, into a Roman numeral,
   -- only without using Roman numerals
   --
   -- more precisely, it works from right to left through B5,
   -- and when it encounters an entry that is greater than 2,
   -- it subtracts 5 from that entry, then adds 1 to the previous entry...
   -- more or less the way Roman numerals work

      Pos: Natural := B5'Last;

   begin

      while Pos > 0 loop

         if B5(Pos) > 2 then
            B5(Pos - 1) := B5(Pos - 1) + 1;
            B5(Pos) := B5(Pos) - 5;
         end if;

         Pos := Pos - 1;

      end loop;

   end Romanize;

   function Decimal_To_SNAFU(N: Long_Long_Integer) return String is
   -- converts a decimal to a SNAFU string

      M     : Long_Long_Integer := N; -- used to manipulate N's value
      D     : Long_Long_Integer := 1; -- smallest power of 5 larger than M
      Q     : Long_Long_Integer;      -- quotient of dividing m by d

      Tmp   : Fifty_Base5s := ( others => 0 ); -- stores Q's
      Pos   : Positive := Tmp'First;           -- working index of Tmp

      Result: String(1..50) := ( others => ' ' );

   begin

      -- find smallest power of 5 that is smaller than N = M
      while D * 5 <= N loop
         D := D * 5;
      end loop;

      loop

         -- get next base-5 quindit (?)
         Q := M / D;
         Tmp(Pos) := Q;

         -- adjust M
         M := M rem D;

         -- are we done? if not, set up for next quindit
         if D = 1 then
            exit;
         end if;
         D := D / 5;
         Pos := Pos + 1;

      end loop;

      -- push quindits right & clear old positions to 0
      Tmp(50 - Pos + 2 .. 50) := Tmp(1 .. Pos - 1);
      Tmp(1 .. Pos) := ( others => 0 );

      -- adjust 3's and 4's to ='s and -'s
      Romanize(Tmp);

      -- copy to result
      Pos := 1;
      while Tmp(Pos) = 0 loop
         Pos := Pos + 1;
      end loop;
      for I in Pos .. 50 loop
         Result(I) := ( case Tmp(I) is
                           when 0 => '0',
                           when 1 => '1',
                           when 2 => '2',
                           when -1 => '-',
                           when -2 => '=',
                           when others => raise Bad_Snafu
                       );
      end loop;

      return Result(Pos..50);

   end Decimal_To_SNAFU;

   -- SECTION
   -- I/O

   Filename: constant String
      := ( if Doing_Example then "example.txt" else "input.txt" );

   Input_File: Text_IO.File_Type;

   procedure Read_Input is
   -- read the SNAFU input

      Sum: Long_Long_Integer := 0;
      Tmp: Long_Long_Integer;

   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop
         declare In_String: String := Text_IO.Get_Line(Input_File);
         begin
            Tmp := SNAFU_To_Decimal(In_String);
            Text_IO.Put_Line(In_String & " -> " & Tmp'Image);
            Sum := Sum + Tmp;
         end;
      end loop;

      Text_IO.Close(Input_File);

      Text_IO.Put_Line("sum is" & Sum'Image);
      Text_IO.Put_Line("SNAFU is " & Decimal_To_SNAFU(Sum));

   end Read_Input;

begin

   Read_Input;

end Main;
