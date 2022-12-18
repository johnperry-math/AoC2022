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

   type Grid_Range is new Integer range -1 .. 21;

   type Grid_Array is array( Grid_Range, Grid_Range, Grid_Range ) of Boolean;

   Grid, Air_Bubble: Grid_Array
      := ( others => ( others => ( others => False ) ) );

   function Has_Lava(X, Y, Z: Grid_Range) return Boolean is ( Grid(X, Y, Z) );

   function Exposed_To_Interior(X, Y, Z: Grid_Range) return Boolean is
      ( Air_Bubble( X, Y, Z ) );

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

   procedure Find_Interior is
   -- finds the interior(s) of the clump of lava,
   -- using a breadth-first search algorithm
   -- (HA HA, YOU DIDN'T FOOL ME INTO DEPTH-FIRST THIS TIME!!!)

      type Position is record
         X, Y, Z: Grid_Range;
      end record;
      -- the type we will enqueue for our BFS

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
            X, Y, Z: Grid_Range;
         begin

            Queue.Dequeue(Pos);
            X := Pos.X; Y := Pos.Y; Z := Pos.Z;

            -- check neighbors that are not diagonal
            if X + 1 in Grid_Range and then not Checked(X + 1, Y, Z)
               and then not Grid(X + 1, Y, Z)
            then
               Checked(X + 1, Y, Z) := True;
               Air_Bubble(X + 1, Y, Z) := False;
               Queue.Enqueue( ( X + 1, Y, Z ) );
            end if;

            if X - 1 in Grid_Range and then not Checked(X - 1, Y, Z)
               and then not Grid(X - 1, Y, Z)
            then
               Checked(X - 1, Y, Z) := True;
               Air_Bubble(X - 1, Y, Z) := False;
               Queue.Enqueue( ( X - 1, Y, Z ) );
            end if;

            if Y + 1 in Grid_Range and then not Checked(X, Y + 1, Z)
               and then not Grid(X, Y + 1, Z)
            then
               Checked(X, Y + 1, Z) := True;
               Air_Bubble(X, Y + 1, Z) := False;
               Queue.Enqueue( ( X, Y + 1, Z ) );
            end if;

            if Y - 1 in Grid_Range and then not Checked(X, Y - 1, Z)
               and then not Grid(X, Y - 1, Z)
            then
               Checked(X, Y - 1, Z) := True;
               Air_Bubble(X, Y - 1, Z) := False;
               Queue.Enqueue( ( X, Y - 1, Z ) );
            end if;

            if Z + 1 in Grid_Range and then not Checked(X, Y, Z + 1)
               and then not Grid(X, Y, Z + 1)
            then
               Checked(X, Y, Z + 1) := True;
               Air_Bubble(X, Y, Z + 1) := False;
               Queue.Enqueue( ( X, Y, Z + 1 ) );
            end if;

            if Z - 1 in Grid_Range and then not Checked(X, Y, Z - 1)
               and then not Grid(X, Y, Z - 1)
            then
               Checked(X, Y, Z - 1) := True;
               Air_Bubble(X, Y, Z - 1) := False;
               Queue.Enqueue( ( X, Y, Z - 1 ) );
            end if;

         end;

      end loop;

   end Find_Interior;

   function Exposed_Sides(X, Y, Z: Grid_Range) return Natural is
   -- counts the number of exposed sides at the given position

      Result: Natural := 0;

   begin

      if Has_Lava(X, Y, Z) then -- don't bother if isn't a piece of lava
         if not Has_Lava(X + 1, Y, Z) then Result := Result + 1; end if;
         if not Has_Lava(X - 1, Y, Z) then Result := Result + 1; end if;
         if not Has_Lava(X, Y + 1, Z) then Result := Result + 1; end if;
         if not Has_Lava(X, Y - 1, Z) then Result := Result + 1; end if;
         if not Has_Lava(X, Y, Z + 1) then Result := Result + 1; end if;
         if not Has_Lava(X, Y, Z - 1) then Result := Result + 1; end if;
      end if;
      return Result;

   end Exposed_Sides;

   function Sides_Exposed_To_Air(X, Y, Z: Grid_Range) return Natural is
   -- counts the number of exterior sides exposed at the given position

      Result: Natural := 0;

   begin

      if Has_Lava(X, Y, Z) then -- don't bother if it isn't a piece of lava

         if not Has_Lava(X + 1, Y, Z)
            and not Exposed_To_Interior(X + 1, Y, Z)
         then
            Result := Result + 1;
         end if;

         if not Has_Lava(X - 1, Y, Z)
            and then not Exposed_To_Interior(X - 1, Y, Z)
         then
            Result := Result + 1;
         end if;

         if not Has_Lava(X, Y + 1, Z)
            and then not Exposed_To_Interior(X, Y + 1, Z)
         then
            Result := Result + 1;
         end if;

         if not Has_Lava(X, Y - 1, Z)
            and then not Exposed_To_Interior(X, Y - 1, Z)
         then
            Result := Result + 1;
         end if;

         if not Has_Lava(X, Y, Z + 1)
            and then not Exposed_To_Interior(X, Y, Z + 1)
         then
            Result := Result + 1;
         end if;

         if not Has_Lava(X, Y, Z - 1)
            and then not Exposed_To_Interior(X, Y, Z - 1)
         then
            Result := Result + 1;
         end if;

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
