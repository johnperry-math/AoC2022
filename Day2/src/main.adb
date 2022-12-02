-- Advent of Code 2022, Day 2
--
-- John Perry
--
-- Rock Paper Scissors
--
-- part 1: determine the strategy guide's score
--
-- part 2: whoops, misinterpreted the strategy, so reinterpret it

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is

   -- SECTION
   -- input-related, part 1

   Filename: constant String := "input.txt";
   package ATIO renames Ada.Text_IO;
   Input_File : ATIO.File_Type;

   -- SECTION
   -- scoring

   type Valid_Move is ( Rock, Paper, Scissors );

   Move_Score: array ( Valid_Move ) of Natural :=
                 (
                  Rock     => 1,
                  Paper    => 2,
                  Scissors => 3
                 );

   type Game_Result is ( Loss, Tie, Win );

   Game_Score: array ( Game_Result ) of Natural :=
                 (
                  Loss => 0,
                  Tie  => 3,
                  Win  => 6
                 );

   subtype Elf_Move is Valid_Move;
   subtype My_Move is Valid_Move;

   function Outcome( Elf: Elf_Move; Human: My_Move ) return Game_Result is
      (
       case Elf is
          when Rock =>
             (
              case Human is
                 when Rock => Tie,
                 when Paper => Win,
                 when Scissors => Loss
             ),
          when Paper =>
             (
              case Human is
                 when Rock => Loss,
                 when Paper => Tie,
                 when Scissors => Win
             ),
          when Scissors =>
             (
              case Human is
                 when Rock => Win,
                 when Paper => Loss,
                 when Scissors => Tie
             )
      );

   function Score(Elf, Human: Valid_Move) return Positive is
      (
       Move_Score( Human ) + Game_Score( Outcome( Elf, Human ) )
      );

   Invalid_Character_Move: exception;

   function Character_To_Move(C: Character) return Valid_Move is
      (
       case C is
          when 'A' | 'X' => Rock,
          when 'B' | 'Y' => Paper,
          when 'C' | 'Z' => Scissors,
          when others => raise Invalid_Character_Move with C'Image
      );

   -- SECTION
   -- input-related, part 2

   type Game_Turn is record
   -- a record of who moved how, and what the _total_ game score was
   -- (burden is on user to update the Running Score, yes it's bad design...)
      Elf          : Elf_Move;
      Human        : My_Move;
      Running_Score: Natural;
   end record;

   package Strategy_Vectors is new Ada.Containers.Vectors
      (
       Index_Type   => Positive,
       Element_Type => Game_Turn
      );

   Strategy: Strategy_Vectors.Vector;

   procedure Read_Data is
      Running_Score: Natural := 0;
   begin
      ATIO.Open(Input_File, ATIO.In_File, Filename);
      loop
         declare
            Tactic   : String := ATIO.Get_Line(Input_File);
            Elf      : Elf_Move := Character_To_Move(Tactic(1));
            Human    : My_Move := Character_To_Move(Tactic(3));
            New_Entry: Game_Turn :=
                         (
                          Elf           => Elf,
                          Human         => Human,
                          Running_Score => Running_Score + Score(Elf, Human)
                         );
         begin
            Running_Score := New_Entry.Running_Score;
            Strategy.Append( New_Entry );
         end;
         if ATIO.End_Of_File(Input_File) then exit; end if;
      end loop;
   end Read_Data;

   -- SECTION
   -- testing, part 1

   Test_Data: Strategy_Vectors.Vector;

   procedure Setup_Test_Data is
   begin
      Test_Data.Append( ( Character_To_Move('A'), Character_To_Move('Y'), 0 ) );
      Test_Data.Append( ( Character_To_Move('B'), Character_To_Move('X'), 0 ) );
      Test_Data.Append( ( Character_To_Move('C'), Character_To_Move('Z'), 0 ) );
      Test_Data(1).Running_Score := Score(Test_Data(1).Elf, Test_Data(1).Human);
      Test_Data(2).Running_Score :=
         Test_Data(1).Running_Score + Score(Test_Data(2).Elf, Test_Data(2).Human);
      Test_Data(3).Running_Score :=
         Test_Data(2).Running_Score + Score(Test_Data(3).Elf, Test_Data(3).Human);
   end;

   procedure Verify_Correctness is
   -- checks out
   begin
      Atio.Put_Line("this value:" & Test_Data.Last_Element.Running_Score'Image
                    & " should be 15");
   end Verify_Correctness;

   -- SECTION
   -- Part 2

   procedure Remap(Strategy: in out Strategy_Vectors.Vector) is
   -- remaps and rescores Strategy according to the corrected interpretation:
   -- human move of rock, paper, scissors meant, respectively, lose, draw, win,
   -- so we just remap the human move based on the elf move & the misunderstood
   -- human move, updating the score as we go
      Play_To_Lose : constant My_Move := Rock;
      Play_To_Draw : constant My_Move := Paper;
      Play_To_Win  : constant My_Move := Scissors;
      Running_Score: Natural := 0;
   begin
      for Tactic of Strategy loop
         Tactic.Human :=
            (
             case Tactic.Human is
                when Play_To_Win =>
                   (
                    case Tactic.Elf is
                       when Rock => Paper,
                       when Paper => Scissors,
                       when Scissors => Rock
                   ),
                when Play_To_Draw => Tactic.Elf,
                when Play_To_Lose =>
                   (
                    case Tactic.Elf is
                       when Rock => Scissors,
                       when Paper => Rock,
                       when Scissors => Paper
                   )
            );
         Running_Score := Running_Score + Score(Tactic.Elf, Tactic.Human);
         Tactic.Running_Score := Running_Score;
         -- turned out useful
         --  ATIO.Put_Line(Tactic.Elf'Image & Tactic.Human'Image & Tactic.Running_Score'Image);
      end loop;
   end Remap;

   -- SECTION
   -- testing, part 2

   procedure Test_Remap is
   begin
      Remap(Test_Data);
      ATIO.Put_Line("this value:" & Test_Data.Last_Element.Running_Score'Image
                    & " should be 12");
   end Test_Remap;

begin

   -- tests (good thing I did this; initially misread part 2!)
   Setup_Test_Data;
   Verify_Correctness;
   Test_Remap;

   -- part 1

   Read_Data;
   ATIO.Put_Line(Strategy.Last_Element.Running_Score'Image);

   -- part 2

   Remap(Strategy);
   ATIO.Put_Line(Strategy.Last_Element.Running_Score'Image);

end Main;
