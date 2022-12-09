-- Advent of Code 2022, Day 9
--
-- John Perry
--
-- Rope Bridge
--
-- part 1: count how many unique positions the tail of a 2-knot rope touches
--
-- part 2: count how many unique positions the tail of a 10-knot rope touches
--

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets; use all type Ada.Containers.Hash_Type;

procedure Main is

   -- SECTION
   -- Movement- and Position-related

   type Direction is ( Up, Down, Left, Right );

   type Movement is record
      Dir: Direction;
      Len: Positive;
   end record;

   package Movement_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Positive, Element_Type => Movement );

   Movements: Movement_Vectors.Vector;

   type Position is record
      X, Y: Integer;
   end record;

   function Position_Hash(P: Position) return Ada.Containers.Hash_Type is
      ( Ada.Containers.Hash_Type( abs(P.X) )
        * Ada.Containers.Hash_Type(32003)
        + Ada.Containers.Hash_Type( abs(P.Y) )
       );

   package Position_Sets is new Ada.Containers.Hashed_Sets
      ( Element_Type => Position, Hash => Position_Hash, Equivalent_Elements => "=" );

   Positions: Position_Sets.Set;

   type Deltas is array(Direction) of Integer;
   Delta_X: Deltas := ( Left => -1, Right => 1, others => 0 );
   Delta_Y: Deltas := ( Up => -1, Down => 1, others => 0 );

   -- SECTION
   -- I/O

   Testing: constant Integer := 3;
   Filename: constant String
      := ( case Testing is
              when 1 => "example.txt",
              when 2 => "example2.txt",
              when others => "input.txt"
         );
   package ATIO renames Ada.Text_IO;
   package ATIIO is new Ada.Text_IO.Integer_IO
      (
       Num => Natural
      );
   Input_File    : ATIO.File_Type;

   Bad_Direction: exception;

   procedure Read_Input is
   -- reads the instructions and stores them in Movements
   begin
      ATIO.Open(Input_File, ATIO.In_File, Filename);
      while not ATIO.End_Of_File(Input_File) loop
         declare
            Instruction: String := ATIO.Get_Line(Input_File);
            Dir        : Direction
               := ( case Instruction(1) is
                    when 'U' => Up,
                    when 'D' => Down,
                    when 'L' => Left,
                    when 'R' => Right,
                    when others => raise Bad_Direction
                   );
            Distance   : Natural;
            Last       : Positive;
            package ATIIO is new ATIO.Integer_IO ( Num => Natural );
         begin
            ATIIO.Get(Instruction(3 .. Instruction'Last), Distance, Last);
            Movements.Append( ( Dir => Dir, Len => Distance ) );
         end;
      end loop;
      ATIO.Close(Input_File);
   end Read_Input;

   -- SECTION
   -- the function

   function Num_Unique_Tail_Pos(Length: Positive) return Natural is
   -- returns the number of unique positions
   -- the tail of a rope with `Length` knots touches
   -- after the head passes through all instructions listed in `Movements`
   --
   -- originally wrote this with just a head and a tail,
   -- then realized an array would take care of this for a rope of 10 knots
   -- by considering each knot the head of the next knot,
   --    -- which would be its tail! --
   -- and then realized that the two functions could be combined as one
   -- that would work for any number of knots

      Knots: array ( 1 .. Length ) of Position -- our roe
         := ( others => ( X => 0, Y => 0 ) );

      Dx, Dy: Integer; -- how each knot moves in comparison

   begin

      Positions.Clear;

      Head_Movements: for Instruction of Movements loop
         Head_Length: for I in 1 .. Instruction.Len loop -- repeat each instruction
                                            -- the given number of times

            if not Positions.Contains(Knots(Length)) then
               Positions.Insert(Knots(Length));
            end if;

            -- move the head
            Knots(1).X := Knots(1).X + Delta_X(Instruction.Dir);
            Knots(1).Y := Knots(1).Y + Delta_Y(Instruction.Dir);

            -- now move the rest of the knots,
            -- considering each as the tail of a two-knot rope
            -- with the preceding knot being the head
            Tails: for I in 1 .. Length - 1 loop

               declare
                  Head: Position renames Knots(I);
                  Tail: Position renames Knots(I + 1);
               begin

                  -- the head only moves one position at a time,
                  -- so we can make certain assumptions about what
                  -- a distance of 2 or 3 mean

                  if abs(Head.X - Tail.X) + abs(Head.Y - Tail.Y) > 2 then
                     -- case 1: the head is a Manhattan distance of at least 3
                     -- previously, the head was a distance of at most 2,
                     -- AND if it was 2 then it was diagonal to it,
                     -- so a distance of 3 is possible only if
                     -- the tail needs to move diagonally
                     --
                     -- this first case was tricky, but the example helped a lot
                     Dx := ( if Head.X < Tail.X then -1 else 1 );
                     Dy := ( if Head.Y < Tail.Y then -1 else 1 );
                     Tail.X := Tail.X + Dx;
                     Tail.Y := Tail.Y + Dy;

                  elsif abs(Head.X - Tail.X) > 1 then
                     -- case 2: the head is a linear distance of at least 2
                     -- previously, the head was a linear distance
                     -- of at most sqrt(2), and if its distance is more than 2
                     -- in a diagonal direction then its Manhattan distance
                     -- will be at least 3, which is the pervious case
                     --
                     -- (it helps that the rules restrict the motions
                     --  to cardinal directions)
                     Dx := ( if Head.X < Tail.X then -1 else 1 );
                     Tail.X := Tail.X + Dx;

                  elsif abs(Head.Y - Tail.Y) > 1 then
                     -- see previous comment
                     DY := ( if Head.Y < Tail.Y then -1 else 1 );
                     Tail.Y := Tail.Y + DY;

                  end if;

               end;

            end loop Tails;

         end loop Head_Length;
      end loop Head_Movements;

      -- in case the tail hit a unique position on the last move
      if not Positions.Contains(Knots(Length)) then
         Positions.Insert(Knots(Length));
      end if;

      return Integer(Positions.Length);

   end Num_Unique_Tail_Pos;

begin

   Read_Input;

   ATIO.Put_Line("Two knots touch" & Num_Unique_Tail_Pos(2)'Image
                 & " at least once"
                );

   ATIO.Put_Line("Ten knots touch" & Num_Unique_Tail_Pos(10)'Image
                 & " at least once"
                );

end Main;
