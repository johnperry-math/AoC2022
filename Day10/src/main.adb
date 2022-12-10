-- Advent of Code 2022, Day 10
--
-- John Perry
--
-- Cathode-Ray Tube
--
-- part 1: find the sum of six signal strengths, where
--    signal strength is defined as the product of register value
--    and cycles passed
--
-- part 2: determine what letters are written to the screen
--

with Ada.Text_IO;

procedure Main is

   -- SECTION
   -- oh boy, a CPU

   type Cycle is new Integer;
   type Register is new Integer;
   type Instructions is ( AddX, NoOp );

   X: Register := 1;
   Cycles_Passed: Cycle := 1;

   -- SECTION
   -- I/O

   Testing: constant Integer := 3;
   Filename: constant String
      := ( case Testing is
              when 1 => "example.txt",
              when 2 => "example2.txt",
              when others => "input.txt"
         );
   package Text_IO renames Ada.Text_IO;
   Input_File    : Text_IO.File_Type;

   package Instr_IO is new Text_IO.Enumeration_IO
   -- I/O for Instructions, mainly for input
      (
       Enum => Instructions
      );

   package Int_IO is new Ada.Text_IO.Integer_IO
   -- I/O for Integers
      (
       Num => Integer
      );

   Bad_Instruction: exception;

   procedure Apply(Operation: Instructions; Value: Register := 0) is
   -- applies `Operation` to register `X` with `Value`, if appropriate
   begin
      case Operation is
      when AddX => X := X + Value;
      when NoOp => null;
      when others => raise Bad_Instruction;
      end case;
   end Apply;

   Crt: array( 0 .. 5 , 0 .. 39 ) of Character -- secret message to appear...
      := ( others => ( others => '.' ) );

   procedure Print_Crt is
      -- displays whatever is in Crt to standard output
   begin
      for Row in Crt'Range(1) loop
         for Col in Crt'Range(2) loop
            Text_IO.Put(Crt(Row, Col));
         end loop;
         Text_IO.New_Line;
      end loop;
   end Print_Crt;

   function Read_Input return Register is
   -- reads the input and takes care of parts 1 and 2 simultaneously

      Signal_Strength: Register := 0;
      -- sum of X * Cycles_Passed when Cycles_Passed = 20, 60, 100, ..., 220

      -- when we read an operation that doesn't complete in the current cycle,
      -- we store it in `Operation` and any corresponding value in `Value`,
      -- then set `Trigger` to be the cycle in which we should perform it
      Operation: Instructions := NoOp;
      Trigger: Cycle := -1;
      Value  : Integer := 0;

   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while Trigger >= Cycles_Passed -- make sure we perform the necessary op
         or not Text_IO.End_Of_File(Input_File)
      loop

         -- update CRT
         if Register(Cycles_Passed - 1) mod 40 in X - 1 .. X + 1 then
            Crt(
                Integer(Cycles_Passed - 1) / 40,
                Integer(Cycles_Passed - 1) mod 40
               ) := '#';
         end if;

         if Cycles_Passed < Trigger then

            -- wait until ready
            null;

         elsif Cycles_Passed = Trigger then

            Apply(Operation, Register(Value));

         elsif not Text_IO.End_Of_File(Input_File) then

            -- read
            Instr_IO.Get(Input_File, Operation);

            case Operation is

               when AddX =>
                  Trigger := Cycles_Passed + 1;
                  Operation := AddX;
                  Int_IO.Get(Input_File, Value);

               when NoOp =>
                  null;

               when others =>
                  raise Bad_Instruction;

            end case;

         end if;

         -- update cycle and `Signal_Strength`
         Cycles_Passed := Cycles_Passed + 1;
         if Cycles_Passed rem 40 = 20 then
            Signal_Strength := Signal_Strength + X * Register(Cycles_Passed);
         end if;

      end loop;

      Text_IO.Close(Input_File);
      return Signal_Strength;

   end Read_Input;

begin

   Text_IO.Put_Line("Signal strength sum is" & Read_Input'Image);
   Print_Crt;

end Main;
