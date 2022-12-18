-- Advent of Code 2022, Day 18
--
-- John Perry
--
-- Boiling Boulders
--
-- part 1: determine the number of exposed sides of falling lava droplets
--
-- part 2: determine the number of exposed **exterior** sides
--

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   -- SUBSECTION
   -- the types and varibles themselves

   type Grid_Range is new Integer range -1 .. 21;

   type Grid_Array is array( Grid_Range, Grid_Range, Grid_Range ) of Boolean;

   Grid, Air_Bubble: Grid_Array
      := ( others => ( others => ( others => False ) ) );

   type Position is record
      X, Y, Z: Grid_Range;
   end record;

   type Position_Array is array(1 .. 6) of Position;

   Directions: Position_Array
      := ( ( 1, 0, 0 ), ( -1, 0, 0 ), ( 0, 1, 0 ), ( 0, -1, 0 ),
           ( 0, 0, 1 ), ( 0, 0, -1 )
          );

   -- SUBSECTION
   -- useful functions

   function Valid_Move(Pos: Position; Dir: Position) return Boolean is
      ( Pos.X + Dir.X in Grid_Range
        and then Pos.Y + Dir.Y in Grid_Range
        and then Pos.Z + Dir.Z in Grid_Range
       );

   function Has_Lava(X, Y, Z: Grid_Range) return Boolean is ( Grid(X, Y, Z) );

   function Exposed_To_Interior(X, Y, Z: Grid_Range) return Boolean is
      ( Air_Bubble( X, Y, Z ) );

   procedure Find_Interior is
   -- finds the interior(s) of the clump of lava,
   -- using a breadth-first search algorithm
   -- (HA HA, YOU DIDN'T FOOL ME INTO DEPTH-FIRST THIS TIME!!!)

      use all type Ada.Containers.Count_Type;
      package Bfs_Queue_Interfaces
      is new Ada.Containers.Synchronized_Queue_Interfaces
            (Element_Type => Position);
      package Bfs_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
         ( Queue_Interfaces => Bfs_Queue_Interfaces );
      Queue: Bfs_Queues.Queue;

      Checked: Grid_Array := ( others => ( others => ( others => False ) ) );
      -- keeps track of the checked positions

   begin
      -- start by marking all that are not lava
      for X in Grid_Range loop
         for Y in Grid_Range loop
            for Z in Grid_Range loop
               Air_Bubble(X, Y, Z) := not Grid(X, Y, Z);
            end loop;
         end loop;
      end loop;

      -- use a bfs to clear anything connected to the outside
      -- we know (-1, -1, -1) has no lava (checked the input...)
      Queue.Enqueue( ( -1, -1, -1 ) );
      Air_Bubble( -1, -1, -1 ) := False;
      Checked( -1, -1, -1 ) := True;

      while Queue.Current_Use > 0 loop

         declare
            Pos: Position;
         begin

            Queue.Dequeue(Pos);

            -- check neighbors that are not diagonal
            for Dir of Directions loop

               if Valid_Move(Pos, Dir) then

                  declare
                     X: Grid_Range := Pos.X + Dir.X;
                     Y: Grid_Range := Pos.Y + Dir.Y;
                     Z: Grid_Range := Pos.Z + Dir.Z;
                  begin

                     if not Checked(X, Y, Z)
                        and then not Grid(X, Y, Z)
                     then
                        Checked(X, Y, Z) := True;
                        Air_Bubble(X, Y, Z) := False;
                        Queue.Enqueue( ( X, Y, Z ) );
                     end if;
                  end;

               end if;


            end loop;

         end;

      end loop;

   end Find_Interior;

   function Exposed_Sides(X, Y, Z: Grid_Range) return Natural is
   -- counts the number of exposed sides at the given position

      Result: Natural := 0;

   begin

      if Has_Lava(X, Y, Z) then -- don't bother if isn't a piece of lava
         for Dir of Directions loop
            if not Has_Lava(X + Dir.X, Y + Dir.Y, Z + Dir.Z) then
               Result := Result + 1;
            end if;
         end loop;
      end if;
      return Result;

   end Exposed_Sides;

   function Sides_Exposed_To_Air(X, Y, Z: Grid_Range) return Natural is
   -- counts the number of exterior sides exposed at the given position

      Result: Natural := 0;

   begin

      if Has_Lava(X, Y, Z) then -- don't bother if it isn't a piece of lava

         for Dir of Directions loop

            if not Has_Lava(X + Dir.X, Y + Dir.Y, Z + Dir.Z)
               and not Exposed_To_Interior(X + Dir.X, Y + Dir.Y, Z + Dir.Z)
            then
               Result := Result + 1;
            end if;

         end loop;

      end if;

      return Result;

   end Sides_Exposed_To_Air;

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
   -- read the lava droplet locations

   begin

   Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop

         declare
            Input_String  : String := Text_IO.Get_Line(Input_File);
            X, Y, Z       : Integer;
            Pos           : Positive := 1;
         begin

            Get_Integer(Input_String, X, Pos); Pos := Pos + 1;
            Get_Integer(Input_String, Y, Pos); Pos := Pos + 1;
            Get_Integer(Input_String, Z, Pos); Pos := Pos + 1;
            Grid(Grid_Range(X), Grid_Range(Y), Grid_Range(Z)) := True;
         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Input;

   procedure Put_Grid(Which: Grid_Array) is
      -- useful for debugging
   begin
      for X in Grid_Range loop
         Text_IO.Put_Line(X'Image);
         for Y in Grid_Range loop
            for Z in Grid_Range loop
               Text_IO.Put( ( if Which(X, Y, Z) then '#' else '.' ) );
            end loop;
            Text_IO.New_Line;
         end loop;
         Text_IO.New_Line(2);
      end loop;
   end;

   -- SECTION
   -- PART 1

   function Surface_Area return Natural is
   -- returns the surface area of the lava blocks

      Result: Natural := 0;

   begin

      for X in Grid_Range'First + 1 .. Grid_Range'Last - 1 loop
         for Y in Grid_Range'First + 1 .. Grid_Range'Last - 1 loop
            for Z in Grid_Range'First + 1 .. Grid_Range'Last - 1 loop

               Result := Result + Exposed_Sides(X, Y, Z);

            end loop;
         end loop;
      end loop;

      return Result;

   end Surface_Area;

   function Surface_Area_To_Water return Natural is
   -- returns the surface area of the exterior blocks of lava

      Result: Integer := 0;

   begin

      -- first find the interior
      Find_Interior;

      for X in Grid_Range'First + 1 .. Grid_Range'Last - 1 loop
         for Y in Grid_Range'First + 1 .. Grid_Range'Last - 1 loop
            for Z in Grid_Range'First + 1 .. Grid_Range'Last - 1 loop

               Result := Result + Sides_Exposed_To_Air(X, Y, Z);

            end loop;
         end loop;
      end loop;

      return Result;

   end Surface_Area_To_Water;

begin

   Read_Input;

   Text_IO.Put_Line("the exposed surface area is" & Surface_Area'Image);
   Text_IO.Put_Line("the surface area exposed to water is"
                    & Surface_Area_To_Water'Image);

end Main;
