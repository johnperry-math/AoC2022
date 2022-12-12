-- Advent of Code 2022, Day 12
--
-- John Perry
--
-- Hill Climbing Algorithm
--
-- part 1: find the shortest route from start to apex
--
-- part 2: find the shortest route from any ground level to apex
--

with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Main is

   package Text_IO renames Ada.Text_IO;

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   -- SUBSECTION
   -- the map

   subtype Elevation is Character range 'a' .. 'z';
   subtype Row_Range is Positive range 1 .. ( if Doing_Example then 5 else 41 );
   subtype Col_Range is Positive range 1 .. ( if Doing_Example then 8 else 161 );
   Elevation_Map: array( Row_Range, Col_Range ) of Elevation;

   -- SUBSECTION
   -- my position

   type Position is record
      Row: Row_Range;
      Col: Col_Range;
   end record;

   -- SUBSECTION
   -- allowed motion

   function Good_Climb(From, To: Position; Part: Part_Number) return Boolean is
      (
       case Part is
          when First => Character'Pos(Elevation_Map(To.Row, To.Col))
       - Character'Pos(Elevation_Map(From.Row, From.Col))
       <= 1,
       when Second => Character'Pos(Elevation_Map(To.Row, To.Col))
       - Character'Pos(Elevation_Map(From.Row, From.Col))
       >= -1
      );

   Start, Apex: Position;

   -- SECTION
   -- I/O

   Filename: constant String
      := ( if Doing_Example then "example.txt" else "input.txt" );

   Input_File: Text_IO.File_Type;

   Unexpected_Character: exception;

   procedure Read_Map is
   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      for Row in Row_Range loop

         declare
            Input_String: String := Text_IO.Get_Line(Input_File);
         begin

            for Col in Col_Range loop

               case Input_String(Col) is

                  when Elevation =>
                     Elevation_Map(Row, Col) := Input_String(Col);

                  when 'S' => -- start position
                     Elevation_Map(Row, Col) := 'a';
                     Start := ( Row => Row, Col => Col );

                  when 'E' => -- end position ("apex")
                     Elevation_Map(Row, Col) := 'z';
                     Apex := ( Row => Row, Col => Col );

                  when others => raise Unexpected_Character
                        with "at" & Row'Image & Col'Image;

               end case;

            end loop;

         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Map;

   -- SECTION
   -- breadth-first search

   -- SUBSECTION
   -- more motion

   type Direction is ( Up, Down, Left, Right );
   type Delta_Array is array ( Direction ) of Integer;
   Delta_Col: Delta_Array := ( 0, 0, -1, 1 );
   Delta_Row: Delta_Array := ( -1, 1, 0, 0 );

   function New_Position(From: Position; Into: Direction) return Position is
   -- returns the position one step from `From` in the direction `Into`
      (
       if From.Row + Delta_Row(Into) in Row_Range
       and then From.Col + Delta_Col(Into) in Col_Range
       then
          ( Row => From.Row + Delta_Row(Into),
            Col => From.Col + Delta_Col(Into)
           )
       else
          From
      );

   -- SUBSECTION
   -- remembering the positions we've passed

   function "<"(Left, Right: Position) return Boolean is
      (
       Left.Row < Right.Row
       or else
          ( Left.Row = Right.Row and then Left.Col < Right.Col )
      );

   package Position_Sets is new Ada.Containers.Ordered_Sets
      (
       Element_Type => Position
      );

   type Position_And_Distance is record
      Where: Position;
      Distance: Natural;
   end record;

   function Find_Shortest_Route(Part: Part_Number := First) return Natural is
   -- uses breadth-first search to find the shortest route
   -- from the starting point to the ending point;
   -- determining the start and end depends on `Part`:
   -- * when `Part = First`, we start at `Start` and end at `Apex`;
   -- * when `Part = Second`, we start at `Apex` and end at any position
   --   that has an `a` on the map

      use all type Ada.Containers.Count_Type;
      -- needed for testing for empty

      Explored: Position_Sets.Set;
      -- positions explored

      package Position_Queue_Interfaces
      is new Ada.Containers.Synchronized_Queue_Interfaces
         (
          Element_Type => Position_And_Distance
         );
      package Position_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
         (
          Queue_Interfaces => Position_Queue_Interfaces
         );
      Position_Queue: Position_Queues.Queue;
      -- positions we still want to explore

      Current: Position_And_Distance;
      -- current position
      Starting_Point: Position := ( if Part = First then Start else Apex );
      -- starting position; depends on which part!

   begin

      Explored.Insert(Starting_Point);
      Position_Queue.Enqueue( ( Starting_Point, 0 ) );

      while Position_Queue.Current_Use > 0 loop

         Position_Queue.Dequeue(Current);

         if ( Part = First and then Current.Where = Apex )
            or else
               ( Part = Second
                  and then Elevation_Map(Current.Where.Row, Current.Where.Col) = 'a'
                 )
         then -- found it!
            return Current.Distance;

         else -- keep looking, in each direction
            for Dir in Direction loop
               declare
                  New_Pos: Position := New_Position(Current.Where, Dir);
               begin
                  if Good_Climb(Current.Where, New_Pos, Part) -- can we go here?
                     and then not Explored.Contains(New_Pos)  -- did we already?
                  then
                     Explored.Insert(New_Pos);
                     Position_Queue.Enqueue( ( New_Pos, Current.Distance + 1 ) );
                  end if;
               end;
            end loop;
         end if;

      end loop;

      return Natural'Last;

   end Find_Shortest_Route;

begin

   Read_Map;

   Text_IO.Put_Line("shortest route from start to apex takes"
                    & Find_Shortest_Route'Image
                   );
   Text_IO.Put_Line("shortest route from apex to ground takes"
                    & Find_Shortest_Route(Second)'Image
                   );

end Main;
