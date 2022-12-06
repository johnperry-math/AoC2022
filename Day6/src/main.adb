-- Advent of Code 2022, Day 6
--
-- John Perry
--
-- Tuning Trouble
--
-- part 1: Find the first marker of four distinct characters.
--
-- part 2: Find the first message of 14 distinct characters.
--
-- not especially challenging today... the terror is yet to come...

with Ada.Text_IO;

procedure Main with SPARK_Mode => On is

   -- SECTION
   -- input-related, part 1

   Filename: constant String := "input.txt";
   package ATIO renames Ada.Text_IO;
   Input_File    : ATIO.File_Type;

   -- SECTION
   -- utility types and subprograms

   subtype Raw_Message is String(1..4096);

   -- SECTION
   -- Part 1

   function Find_Marker(S: Raw_Message; Length: Positive) return Positive
      with Pre => Length < Positive'Last
   is
   -- find the first sequence of four distinct character and
   -- return the position of the first of these characters
      Temp     : String(1..Length) := ( others => ' ' );
      Temp_Pos : Natural := 1;
      S_Pos    : Positive := 1;
   begin
      while S_Pos <= S'Length and then Temp_Pos < Length + 1 loop
         pragma Loop_Invariant(S_Pos in S'Range);
         while Temp_Pos < Temp'Last + 1
            and then ( for some I in 1 .. Temp_Pos - 1 => Temp(I) = S(S_Pos) )
         loop
            Temp(1 .. Temp_Pos - 2) := Temp(2 .. Temp_Pos - 1);
            Temp(Temp_Pos - 1) := S(S_Pos);
            Temp_Pos := Temp_Pos - 1;
         end loop;
         if Temp_Pos > 0 and then Temp_Pos <= Temp'Last then
            Temp(Temp_Pos) := S(S_Pos);
            Temp_Pos := Temp_Pos + 1;
            S_Pos := S_Pos + 1;
         else
            -- I'd rather return an exception, but SPARK doesn't allow that
            return Positive'Last;
         end if;
      end loop;
      if S_Pos >= 2 then
         return S_Pos - 1;
      else
         -- I'd rather return an exception, but SPARK doesn't allow that
         return Positive'Last;
      end if;
   end Find_Marker;

   function To_Raw_Message(S: String) return Raw_Message
      with Pre => S'First = Raw_Message'First
      and then S'Last <= Raw_Message'Last,
         Post => ( for all I in S'Range => To_Raw_Message'Result(I) = S(I) )
   is
      Result: Raw_Message := ( others => ' ' );
   begin
      for I in S'Range loop
         Result(I) := S(I);
         pragma Loop_Invariant( for all J in S'First .. I => Result(J) = S(J) );
      end loop;
      return Result;
   end To_Raw_Message;

   Test_1: String := "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
   Test_2: String := "bvwbjplbgvbhsrlpgdmjqwftvncz";
   Test_3: String := "nppdvjthqldpwncqszvftbrmjlhg";
   Test_4: String := "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
   Test_5: String := "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";

begin

   -- warmup

   ATIO.Put_Line(Test_1 & Find_Marker(To_Raw_Message(Test_1), 4)'Image);
   ATIO.Put_Line(Test_2 & Find_Marker(To_Raw_Message(Test_2), 4)'Image);
   ATIO.Put_Line(Test_3 & Find_Marker(To_Raw_Message(Test_3), 4)'Image);
   ATIO.Put_Line(Test_4 & Find_Marker(To_Raw_Message(Test_4), 4)'Image);
   ATIO.Put_Line(Test_5 & Find_Marker(To_Raw_Message(Test_5), 4)'Image);

   ATIO.Put_Line(Test_1 & Find_Marker(To_Raw_Message(Test_1), 14)'Image);
   ATIO.Put_Line(Test_2 & Find_Marker(To_Raw_Message(Test_2), 14)'Image);
   ATIO.Put_Line(Test_3 & Find_Marker(To_Raw_Message(Test_3), 14)'Image);
   ATIO.Put_Line(Test_4 & Find_Marker(To_Raw_Message(Test_4), 14)'Image);
   ATIO.Put_Line(Test_5 & Find_Marker(To_Raw_Message(Test_5), 14)'Image);

   -- parts 1 and 2
   ATIO.Open(Input_File, ATIO.In_File, Filename);
   declare
      Input_String: Raw_Message;
      Last: Natural;
   begin
      ATIO.Get_Line(Input_File, Input_String, Last);
      ATIO.Put_Line("Part 1" & Find_Marker(Input_String, 4)'Image);
      ATIO.Put_Line("Part 2" & Find_Marker(Input_String, 14)'Image);
   end;
   ATIO.Close(Input_File);


end Main;
