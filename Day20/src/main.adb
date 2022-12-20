-- Advent of Code 2022, Day 20
--
-- John Perry
--
-- Grove Positioning System
--
-- part 1: reorder a circular list depending on the number's value
--
-- part 2: multiply the values by a ginormous number, then reorder 10 times
--

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   package Integer_Lists is new Ada.Containers.Vectors
      (
       Index_Type => Positive,
       Element_Type => Integer
      );

   Input_List: Integer_Lists.Vector;

   -- SECTION
   -- I/O

   Filename: constant String
      := ( if Doing_Example then "example.txt" else "input.txt" );

   Input_File: Text_IO.File_Type;

   procedure Get_Integer(S: String; Result: out Integer; Pos: in out Positive)
   is
   -- Ada.Text_IO.Integer_IO has issues with "(\d)*:",
   -- so I have to roll my own

      Is_Negative: Boolean := S(Pos) = '-';

   begin

      if Is_Negative then Pos := Pos + 1; end if;

      Result := 0;
      while Pos <= S'Length and then S(Pos) in '0'..'9' loop
         Result := Result * 10;
         Result := Result + Character'Pos(S(Pos)) - Character'Pos('0');
         Pos := Pos + 1;
      end loop;

      if Is_Negative then Result := -Result; end if;

   end Get_Integer;

   procedure Read_Input is
   -- read the blueprints

      Blueprints_Read: Natural := 0;

   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop
         declare
            Input_String: String := Text_IO.Get_Line(Input_File);
            Value       : Integer;
            Pos         : Positive := 1;
         begin
            Get_Integer(Input_String, Value, Pos);
            Input_List.Append(Value);
         end;
      end loop;

      Text_IO.Close(Input_File);

   end Read_Input;

   procedure Put_Reordered_List(Original, Positions: Integer_Lists.Vector) is
   -- I attacked today's puzzle using two lists; the original, and
   -- another list that contained the elements' new positions; that is,
   -- Positions(I) tells me the new index of Original(I)
   -- this procedure prints out the resulting list,
   -- and was useful for debugging

   begin

      for I in 1 .. Positive(Original.Length) loop
         Text_IO.Put(" " & Integer'Image(Original(Positions(I))));
      end loop;
      Text_IO.New_Line;

   end Put_Reordered_List;

   -- SECTION
   -- PART 1

   function Sum_Of_Coordinates return Integer is
   -- determines the result of sorting Input_List
   -- by shifting each value around the same number of places as its value,
   -- then returns the sum of the 1000th, 2000th, and 3000th entries
   -- of the final list

      use type Ada.Containers.Count_Type;

      Result       : Integer := 0;
      Position_List: Integer_Lists.Vector;

   begin

      -- set up the position list
      Position_List.Set_Length(Position_List.Length);
      for I in 1 .. Positive(Input_List.Length) loop
         Position_List.Append(I);
      end loop;

      -- reorder it
      Through_Values: for K in 1 .. Positive(Position_List.Length) loop

         -- if the value at position k is larger than the list's length,
         -- there's no point cycling around however many times;
         -- might as well work out the resulting position after all those cycles

         declare
            Value     : Integer := Input_List(K); -- item to move
            I, J      : Natural := Position_List.Find_Index(K); -- current loc'n
            Net_Motion: Integer := abs(Value) rem
               Positive( Position_List.Length - 1 ); -- how far it moves

         begin

            -- if the value goes past the end of the list,
            -- it will be a lot easier to handle it as motion
            -- in the opposite direction

            if Value > 0 and then Net_Motion >= (Positive(Position_List.Length))
            then
               Net_Motion := Positive(Position_List.Length) - Net_Motion - 1;
               Value := -Value;

            elsif Value < 0 and then Net_Motion + 1 >= I then
               Net_Motion := Positive(Position_List.Length) - Net_Motion - 1;
               Value := -Value;

            end if;

            -- perform the shift

            Through_Shift: for L in 1 .. abs(Net_Motion) loop

               if Value < 0 then
                  J := J - 1;
                  if J = 1 then J := Positive( Position_List.Length ); end if;
                  Position_List.Swap(I, J);
                  I := J;

               else
                  J := J + 1;
                  if J > Positive( Position_List.Length ) then J := 1; end if;
                  Position_List.Swap(I, J);
                  I := J;

               end if;

            end loop Through_Shift;

         end;

      end loop Through_Values;

      declare
         K: Positive := Input_List.Find_Index(0);
         J: Positive := Position_List.Find_Index(K);
      begin

         for I in 1 .. 3000 loop
            J := J + 1;
            if I rem 1000 = 0 then
               Result := Result
                  + Input_List(Position_List
                               (J rem Positive(Position_List.Length))
                              );
            end if;
         end loop;

      end;

      return Result;

   end Sum_of_Coordinates;

   -- SECTION
   -- PART 2

   function Sum_Of_New_Coordinates return Long_Long_Integer is
   -- same as Sum_of_Coordinates, except that
   -- now we have to multiply the values by some ludicrously large number,
   -- then sort it 10 times
   --
   -- since the solution to part 1 already uses remainders,
   -- this doesn't slow us down that much here
   --
   -- I thought about taking the remainder of Key by the list's length
   -- so as to avoid using Long_Long_Integer, but
   -- (a) that would complicate things, and
   -- (b) I was already using a remainder for the part where it matters most,
   --     and
   -- (c) the result is (probably) a Long_Long_Integer anyway,
   --     so I can't avoid it even then

      use type Ada.Containers.Count_Type;

      Key: constant Long_Long_Integer := 811589153;

      package Long_Integer_Lists is new Ada.Containers.Vectors
            ( Index_Type => Positive,
              Element_Type => Long_Long_Integer
             );

      Result       : Long_Long_Integer := 0;
      Long_List    : Long_Integer_Lists.Vector;
      Position_List: Integer_Lists.Vector;

   begin

      -- set up the position list
      Position_List.Set_Length(Position_List.Length);
      for I in 1 .. Positive(Input_List.Length) loop
         Position_List.Append(I);
         -- if I tried Long_Long_Integer( Input_List.Element(I) * Key ),
         -- gnat complained about an ambiguous name or some such nonsense
         declare New_Entry: Long_Long_Integer
               := Long_Long_Integer( Input_List.Element(I) );
         begin
            Long_List.Append( New_Entry * Key );
         end;
      end loop;

      Loop_10: for Each in 1 .. 10 loop

         Through_Shift: for K in 1 .. Positive(Position_List.Length) loop

            declare
               Value     : Long_Long_Integer := Long_List(K);
               I, J      : Natural := Position_List.Find_Index(K);
               Net_Motion: Long_Long_Integer := abs(Value) rem
                  Long_Long_Integer( Position_List.Length - 1 );
            begin

               if Value > 0
                  and then Net_Motion >= Long_Long_Integer(Position_List.Length)
               then
                  Net_Motion := Long_Long_Integer(Position_List.Length)
                     - Net_Motion - 1;
                  Value := -Value;

               elsif Value < 0 and then Net_Motion + 1 >= Long_Long_Integer(I) then
                  Net_Motion := Long_Long_Integer(Position_List.Length)
                     - Net_Motion - 1;
                  Value := -Value;

               end if;

               Through_Values: for L in 1 .. abs(Net_Motion) loop

                  if Value < 0 then
                     J := J - 1;
                     if J = 1 then J := Positive( Position_List.Length ); end if;
                     Position_List.Swap(I, J);
                     I := J;

                  else
                     J := J + 1;
                     if J > Positive( Position_List.Length ) then J := 1; end if;
                     Position_List.Swap(I, J);
                     I := J;

                  end if;

               end loop Through_Values;

            end;

         end loop Through_Shift;

      end loop Loop_10;

      declare
         K: Positive := Long_List.Find_Index(0);
         J: Positive := Position_List.Find_Index(K);
      begin

         for I in 1 .. 3000 loop
            J := J + 1;
            if I rem 1000 = 0 then
               --  Text_IO.Put_Line(J'Image & Natural'Image(J rem Positive(Position_List.Length)));
               --  Text_IO.Put_Line(Integer'Image(Long_List(Position_List(J rem Positive(Position_List.Length)))));
               Result := Result + Long_List(Position_List(J rem Positive(Position_List.Length)));
            end if;
         end loop;

      end;

      return Result;

   end Sum_Of_New_Coordinates;

begin

   Read_Input;

   Text_IO.Put_Line("part 1:" & Sum_Of_Coordinates'Image);
   Text_IO.Put_Line("part 2:" & Sum_Of_New_Coordinates'Image);

end Main;
