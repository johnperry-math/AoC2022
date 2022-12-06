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

procedure Main is

   -- SECTION
   -- input-related, part 1

   Filename: constant String := "input.txt";
   package ATIO renames Ada.Text_IO;
   Input_File    : ATIO.File_Type;

   -- SECTION
   -- utility types and subprograms

   -- SECTION
   -- Part 1

   function Find_Marker(S: String) return Positive is
   -- find the first sequence of four distinct character and
   -- return the position of the first of these characters
      Temp: String(1..4) := ( others => ' ' );
      S_Pos, Temp_Pos: Natural := 1;
   begin
      while S_Pos < S'Length and Temp_Pos < 5 loop
         while ( for some I in 1 .. Temp_Pos - 1 => Temp(I) = S(S_Pos) ) loop
            Temp(1 .. Temp_Pos - 2) := Temp(2 .. Temp_Pos - 1);
            Temp(Temp_Pos - 1) := S(S_Pos);
            Temp_Pos := Temp_Pos - 1;
         end loop;
         Temp(Temp_Pos) := S(S_Pos);
         Temp_Pos := Temp_Pos + 1;
         S_Pos := S_Pos + 1;
      end loop;
      return S_Pos - 1;
   end Find_Marker;

   function Find_Message(S: String) return Positive is
   -- find the first sequence of fourteen distinct character and
   -- return the position of the first of these characters
      Temp: String(1..14) := ( others => ' ' );
      S_Pos, Temp_Pos: Natural := 1;
   begin
      while S_Pos < S'Length and Temp_Pos < 15 loop
         while ( for some I in 1 .. Temp_Pos - 1 => Temp(I) = S(S_Pos) ) loop
            Temp(1 .. Temp_Pos - 2) := Temp(2 .. Temp_Pos - 1);
            Temp(Temp_Pos - 1) := S(S_Pos);
            Temp_Pos := Temp_Pos - 1;
         end loop;
         Temp(Temp_Pos) := S(S_Pos);
         Temp_Pos := Temp_Pos + 1;
         S_Pos := S_Pos + 1;
      end loop;
      return S_Pos - 1;
   end Find_Message;

begin

   -- warmup

   ATIO.Put_Line(Find_Marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb")'Image);
   ATIO.Put_Line(Find_Marker("bvwbjplbgvbhsrlpgdmjqwftvncz")'Image);
   ATIO.Put_Line(Find_Marker("nppdvjthqldpwncqszvftbrmjlhg")'Image);
   ATIO.Put_Line(Find_Marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")'Image);
   ATIO.Put_Line(Find_Marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")'Image);

   ATIO.Put_Line(Find_Message("mjqjpqmgbljsphdztnvjfqwrcgsmlb")'Image);
   ATIO.Put_Line(Find_Message("bvwbjplbgvbhsrlpgdmjqwftvncz")'Image);
   ATIO.Put_Line(Find_Message("nppdvjthqldpwncqszvftbrmjlhg")'Image);
   ATIO.Put_Line(Find_Message("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")'Image);
   ATIO.Put_Line(Find_Message("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")'Image);

   -- parts 1 and 2
   ATIO.Open(Input_File, ATIO.In_File, Filename);
   declare
      Input_String: String := ATIO.Get_Line(Input_File);
   begin
      ATIO.Put_Line(Find_Marker(Input_String)'Image);
      ATIO.Put_Line(Find_Message(Input_String)'Image);
   end;
   ATIO.Close(Input_File);


end Main;
