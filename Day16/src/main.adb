-- Advent of Code 2022, Day 16
--
-- John Perry
--
-- Proboscidea Volcanium
--
-- part 1: determine the maximum flow you can open through some valves
--
-- part 2: same, but with an elephant's help
--
-- someone please beat me the next time I want to try a depth-first search...

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
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
   -- valves

   subtype Name_Character is Character range 'A' .. 'Z';

   type Valve_Name is record
      Name1, Name2: Name_Character;
   end record;

   -- SUBSECTION
   -- valve containers

   -- SUBSUBSECTION
   -- setting up packages

   use all type Ada.Containers.Count_Type;

   package Valve_Name_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Ada.Containers.Count_Type,
        Element_Type => Valve_Name
       );

   function "<"(Left, Right: Valve_Name) return Boolean is
   -- lexicographic order
      ( Left.Name1 < Right.Name1
        or else ( Left.Name1 = Right.Name1 and then Left.Name2 < Right.Name2 )
       );

   function "="(Left, Right: Valve_Name_Vectors.Vector) return Boolean is
      (
       Left.Length = Right.Length and then
          ( for all I in 1 .. Left.Length => Left(I) = Right(I) )
      );

   package Valve_Flow_Maps is new Ada.Containers.Ordered_Maps
      ( Key_Type => Valve_Name,
        Element_Type => Natural
       );

   package Valve_Name_Sets is new Ada.Containers.Ordered_Sets
      ( Element_Type => Valve_Name );

   package Valve_Tunnel_Maps is new Ada.Containers.Ordered_Maps
      (
       Key_Type => Valve_Name,
       Element_Type => Valve_Name_Vectors.Vector
      );

   package Valve_Travel_Destination_Time_Maps is new Ada.Containers.Ordered_Maps
      ( Key_Type    => Valve_Name,
        Element_Type => Natural
       );

   function "="(Left, Right: Valve_Travel_Destination_Time_Maps.Map)
                return Boolean
   is
      (
       Left.Length = Right.Length -- I don't actually need this so I don't care
      );

   package Valve_Travel_Maps is new Ada.Containers.Ordered_Maps
      (
       Key_Type     => Valve_Name,
       Element_Type => Valve_Travel_Destination_Time_Maps.Map
      );

   -- SUBSUBSECTION
   -- finally, the good stuff

   All_Valves: Valve_Name_Vectors.Vector;
   -- list of all valves

   Valve_Flows: Valve_Flow_Maps.Map;
   -- each valve's flow

   Valve_Tunnels: Valve_Tunnel_Maps.Map;
   -- how the valves are connected:
   -- if Valve_Tunnels(A).Contains(B), then we can travel from A to B

   Valve_Travel_Times: Valve_Travel_Maps.Map;
   -- for each valve V, for each valve V'
   -- this lists the shortest distance from V to V'

   -- SUBSECTION
   -- some functions I couldn't put previously

   function Greater_Flow(Left, Right: Valve_Name) return Boolean is
      ( Valve_Flows(Left) > Valve_Flows(Right) );

   package Sort_By_Flow is new Valve_Name_Vectors.Generic_Sorting
      ( "<" => Greater_Flow );

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

   procedure Read_Valves is
   -- read the sensor / beacon data

   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop

         declare
            Input_String  : String := Text_IO.Get_Line(Input_File);
            Pos           : Positive;
            Flow          : Natural;
            New_Valve     : Valve_Name;
            New_Connection: Valve_Name;
            Connections   : Valve_Name_Vectors.Vector;
         begin

            -- get the name
            New_Valve.Name1 := Input_String(7);
            New_Valve.Name2 := Input_String(8);

            All_Valves.Append(New_Valve);

            Pos := 24;
            Get_Integer(Input_String, Flow, Pos);

            Pos := Pos + 24;
            if not ( Input_String(Pos) in Name_Character ) then
               Pos := Pos + 1;
            end if;
            while Pos <= Input_String'Last loop
               New_Connection.Name1 := Input_String(Pos);
               New_Connection.Name2 := Input_String(Pos + 1);
               Pos := Pos + 4;
               Connections.Append( ( New_Connection ) );
            end loop;

            Valve_Flows.Include( New_Valve, Flow );
            Valve_Tunnels.Include( New_Valve, Connections );

         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Valves;

   procedure Put_Valve(Name: Valve_Name) is
   -- print valve's name to standard output
   begin
      Text_IO.Put(Name.Name1); Text_IO.Put(Name.Name2);
   end Put_Valve;

   procedure Put_Path(Path: Valve_Name_Vectors.Vector) is
   begin
      for Valve of Path loop
         Text_IO.Put(Valve.Name1); Text_IO.Put(Valve.Name2); Text_IO.Put(" ");
      end loop;
      Text_IO.New_Line;
   end Put_Path;

   procedure Put_Travel_Times(Times: Valve_Travel_Destination_Time_Maps.Map) is
   -- format: "(valve_name) -> (shortest time to reach it)", followed by newline
   -- (this assumes that `Times` is set up correctly)
   -- it does NOT print the source valve, so if you want that,
   -- then you have to do it first
   -- (you probably want it)
   --
   -- see the parameter-less Put_Travel_Times below

   begin

      for Name of All_Valves loop
         if Times.Contains(Name) then
            Text_IO.Put(Name.Name1); Text_IO.Put(Name.Name2);
            Text_IO.Put(" -> ");
            Text_IO.Put(Natural'Image(Times(Name))); Text_IO.Put("; ");
         end if;
      end loop;

      Text_IO.New_Line;

   end Put_Travel_Times;

   procedure Put_Travel_Times is
   -- lists all valves known to `All_Valves`,
   -- then prints destinations and shortest travel_times
   -- in the format given by
   --    `Put_Travel_Times(Valve_Travel_Destination_Time_Maps.Map)`

   begin

      for Name of All_Valves loop
         Text_IO.Put(Name.Name1); Text_IO.Put(Name.Name2); Text_IO.Put(": ");
         Put_Travel_Times(Valve_Travel_Times(Name));
      end loop;

   end Put_Travel_Times;

   procedure Put_Path_Set(Path: Valve_Name_Sets.Set) is
   begin
      Text_IO.Put("{ ");
      for Valve of Path loop
         Put_Valve(Valve); Text_IO.Put(", ");
      end loop;
      Text_IO.Put("}");
   end Put_Path_Set;

   -- SECTION
   -- functions useful for both parts

   procedure Find_Min_Travel_Times(From: Valve_Name) is
   -- finds the minimum travel times to all destinations reachable from `From`
   -- uses breadth-first search
   -- at least my head was screwed on straight for this step

      -- setting up a bfs queue

      type Valve_And_Time is record
         Name: Valve_Name;
         Time : Natural;
      end record;

      -- it really bugs me that it takes 5 lines to set up a queue in Ada
      -- when ever other language would take at most... 1
      -- (I could see 2, but 5?!?)
      package Queued_Valve_Interfaces
      is new Ada.Containers.Synchronized_Queue_Interfaces
         ( Element_Type => Valve_And_Time );
      package Queued_Valves_Queues
      is new Ada.Containers.Unbounded_Synchronized_Queues
         ( Queue_Interfaces => Queued_Valve_Interfaces );
      Queued_Valves: Queued_Valves_Queues.Queue;
      -- valves queued for exploration

      Explored: Valve_Name_Sets.Set; -- valves we've explored
      New_Map: Valve_Travel_Destination_Time_Maps.Map; -- to set up a new record

   begin

      -- set up `From`
      Explored.Include(From);
      Valve_Travel_Times.Include(From, New_Map);
      Valve_Travel_Times(From).Include(From, 0);

      -- prime the queue
      for Valve of Valve_Tunnels(From) loop
         Queued_Valves.Enqueue( ( Valve, 1 ) );
         Valve_Travel_Times(From).Include(Valve, 1);
         Explored.Include(Valve);
      end loop;

      -- perform the search
      Search: while Queued_Valves.Current_Use > 0 loop

         declare
            Valve: Valve_And_Time;
         begin

            Queued_Valves.Dequeue(Valve);

            Map: for Destination of Valve_Tunnels(Valve.Name) loop
               if not Explored.Contains(Destination) then
                  Explored.Include(Destination);
                  Valve_Travel_Times(From).Include(Destination, Valve.Time + 1);
                  Queued_Valves.Enqueue( ( Destination, Valve.Time + 1 ) );
               end if;
            end loop Map;

         end;

      end loop Search;

   end Find_Min_Travel_Times;

   procedure Setup_Travel_Times is
   -- sets up the shorest travel times for all valves
   -- effectively, calls Find_Min_Travel_Times for each valve
   begin
      for Valve of All_Valves loop
         Find_Min_Travel_Times(Valve);
      end loop;
   end Setup_Travel_Times;

   function Expected_Value(From, Dest: Valve_Name; Current_Time: Natural)
                           return Natural
   -- computes the expected value gained from traveling between the given valves
   -- at the given time
   is
      Travel_Time: Natural := Valve_Travel_Times(From)(Dest);
      Flow: Natural := Valve_Flows(Dest);
   begin
      return ( Natural'Max(0, ( 30 - ( Current_Time + Travel_Time) ) * Flow ) );
   end;

   function Max_Benefit
      (From: Valve_Name;
       Current_Path: Valve_Name_Vectors.Vector;
       Current_Time: Natural
      )
       return Natural
   is
   -- used to prune useless connections
   -- works well in part 1 (compared to not using it, anyway)

      Test_Value: Natural := 0;

   begin

      for Dest of All_Valves loop
         if Valve_Flows(Dest) > 0 and then not Current_Path.Contains(Dest)
         then
            Test_Value := Test_Value + Expected_Value(From, Dest, Current_Time);
         end if;
      end loop;

      return Test_Value;

   end Max_Benefit;

   -- SECTION
   -- Part 1

   function Flow_From
      (From        : Valve_Name;
       Current_Time: Natural;
       Current_Flow: Natural;
       Currently_On: Valve_Name_Vectors.Vector
      )
       return Natural
   is
   -- the maximum flow you can open within the given time,
   -- starting from the given valve,
   -- given the current flow and previously opened valves
   --
   -- depth-first search, so it's pretty daggum slow; hence the output

      Result: Natural := Current_Flow;
      Last_Flow: Natural := 0;

   begin

      Text_IO.Put("time" & Current_Time'Image & ": ");
      Put_Path(Currently_On);

      Through_All_Valves: for Dest of All_Valves loop

         if Valve_Flows(Dest) > 0
            and then not Currently_On.Contains(Dest)
         then

            declare
               Next_On: Valve_Name_Vectors.Vector := Currently_On;
            begin

               if Max_Benefit(Dest, Currently_On, Current_Time) > Last_Flow then
                  Next_On.Append(Dest);
                  Result := Natural'Max
                     (Result,
                      Flow_From
                         (Dest,
                          Current_Time + Valve_Travel_Times(From)(Dest) + 1,
                          Current_Flow
                          + Expected_Value(From, Dest, Current_Time),
                          Next_On
                         )
                     );
                  Last_Flow := Result - Current_Flow;

               end if;

            end;

         end if;

      end loop Through_All_Valves;

      return Result;

   end Flow_From;

   -- PART 2, TAKE 1
   -- depth-first search. takes hours and hours and hours
   -- hopefully not days and days and days

   type Both_Flows is record
      H_Flow, E_Flow: Natural := 0;
   end record;
   -- human-generated flow and elephant-generated flow

   function Flow_With_Elephant_From
      (From, Elephant_From        : Valve_Name;
       Current_Time, Elephant_Time: Natural;
       Current_Flow, Elephant_Flow: Natural;
       Currently_On, Elephant_On  : Valve_Name_Vectors.Vector
      )
       return Both_Flows
   is
   -- should return the total flow generated
   -- when the human and elephant start from the indicated places,
   -- at the indicated times,
   -- having generated the indicated flow,
   -- having opened the indicated valves

      Result,              -- final result
         Temp: Both_Flows; -- temporary result from recursion

   begin

      Result := ( Current_Flow, Elephant_Flow );

      Through_All_Valves: for Dest of All_Valves loop

         if Valve_Flows(Dest) > 0
            and then not Currently_On.Contains(Dest)
            and then not Elephant_On.Contains(Dest)
         then

            declare
               Next_On: Valve_Name_Vectors.Vector;
            begin

               -- should the human move to Dest?
               if Current_Time <= Elephant_Time
                  and then Current_Time
                     + Valve_Travel_Times(From)(Dest) + 1 <= 30
               then

                  Next_On := Currently_On;
                  Next_On.Append(Dest);
                  Temp := Flow_With_Elephant_From
                     (Dest,
                      Elephant_From,
                      Current_Time + Valve_Travel_Times(From)(Dest) + 1,
                      Elephant_Time,
                      Current_Flow + Expected_Value(From, Dest, Current_Time),
                      Elephant_Flow,
                      Next_On,
                      Elephant_On
                     );

                  if Temp.H_Flow + Temp.E_Flow > Result.H_Flow + Result.E_Flow
                  then
                     Result := Temp;
                  end if;

                  -- should the elephant move to Dest?
               elsif Elephant_Time
                  + Valve_Travel_Times(Elephant_From)(Dest) + 1 <= 30
               then

                  Next_On := Elephant_On;
                  Next_On.Append(Dest);
                  Temp := Flow_With_Elephant_From
                     (From,
                      Dest,
                      Current_Time,
                      Elephant_Time + Valve_Travel_Times(Elephant_From)(Dest) + 1,
                      Current_Flow,
                      Elephant_Flow + Expected_Value(Elephant_From, Dest, Elephant_Time),
                      Currently_On,
                      Next_On
                     );

                  if Temp.H_Flow + Temp.E_Flow > Result.H_Flow + Result.E_Flow
                  then
                     Result := Temp;
                  end if;

               end if;

            end;

         end if;

      end loop Through_All_Valves;

      return Result;

   end Flow_With_Elephant_From;

   function Max_Pressure(Part: Part_Number) return Natural is
   -- depth-first-search for part 1 or 2

      Position       : Valve_Name := ( 'A', 'A' );
      Currently_On,
         Elephant_On : Valve_Name_Vectors.Vector;
      Part_Two_Result: Both_Flows;

   begin

      -- set up positions
      Currently_On.Append(Position);
      Elephant_On.Append(Position);

      if Part = First then
         return Flow_From(Position, 1, 0, Currently_On);

      else
         Part_Two_Result := Flow_With_Elephant_From
            (Position, Position, 5, 5, 0, 0, Currently_On, Elephant_On);
         return Part_Two_Result.H_Flow + Part_Two_Result.E_Flow;

      end if;

   end Max_Pressure;

   -- SECTION
   -- Part 2 with breadth-first search

   -- SUBSECTION
   -- new packages and types specific to this attempt

   function Name_Hash(Name: Valve_Name) return Ada.Containers.Hash_Type
   is ( Ada.Containers.Hash_Type(
        Character'Pos(Name.Name1) * 15
        + Character'Pos(Name.Name2) )
       );

   function Path_Hash(Path: Valve_Name_Sets.Set) return Ada.Containers.Hash_Type
   is
      Result: Ada.Containers.Hash_Type := 0;
      use all type Ada.Containers.Hash_Type;
   begin
      for Valve of Path loop
         Result := ( Result * 15 * 15 rem Ada.Containers.Hash_Type'Last )
                     + Name_Hash(Valve);
      end loop;
      return Result;
   end Path_Hash;

   function Equivalent_Valve_Sets(Left, Right: Valve_Name_Sets.Set)
                            return Boolean
   is ( ( Left.Length = Right.Length )
        and then
           ( for all Valve of Left => Right.Contains(Valve) )
       );

   type Path_Record is record
      Name: Valve_Name;
      Flow: Natural;
      Time_Remaining: Natural;
      Traversed: Valve_Name_Sets.Set;
   end record;
   -- used for the breadth-first search of all paths

   package Path_Score_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type        => Valve_Name_Sets.Set,
       Element_Type    => Natural,
       Hash            => Path_Hash,
       Equivalent_Keys => Equivalent_Valve_Sets
      );

   -- again with the complicated queues...
   package Elephant_Interfaces
   is new Ada.Containers.Synchronized_Queue_Interfaces
      ( Element_Type => Path_Record );

   package Elephant_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
      ( Queue_Interfaces => Elephant_Interfaces );

   function Intersects(Path1, Path2: Valve_Name_Sets.Set) return Boolean is
      (
       ( for some Valve of Path1 => Path2.Contains(Valve) )
       and then
       ( for some Valve of Path2 => Path1.Contains(Valve) )
      );

   -- SUBSECTION
   -- at last, the solution

   function Elephant_Bfs return Natural is
   -- a breath of fresh air
   -- er, I mean, a breadth-first solution to part 2
   -- probably adaptable to part 1
   -- adapted from a Python solution
   -- this takes roughly the same amount of time, which says something
   -- about (a) how well I program,
   -- and (b) the careful knitting of the Python runtime and its type system

      Paths: Path_Score_Maps.Map;
      Path_Queue: Elephant_Queues.Queue;
      Empty_Set: Valve_Name_Sets.Set;
      Result    : Natural := 0;

   begin

      Path_Queue.Enqueue(( Name => ( 'A', 'A' ),
                           Flow => 0,
                           Time_Remaining => 26,
                           Traversed => Empty_Set
                          ));

      Through_Queue: while Path_Queue.Current_Use > 0 loop

         declare
            Path         : Path_Record; -- the path we're considering

            Neighbors    : Valve_Name_Sets.Set; -- neighbors of the path's loc
            Neighbor     : Valve_Name; -- guess :-)
            Time         : Natural; -- time from current location to neighbor
            New_Traversed: Valve_Name_Sets.Set; -- next path to enqueue
            New_Flow     : Natural; -- flow generated by adding neighbor to path
                                    -- i.e., New_Traversed's flow

         begin

            Path_Queue.Dequeue(Path);

            Text_IO.Put_Line("considering ");
            Put_Valve(Path.Name); Text_IO.Put(Path.Flow'Image);
            Text_IO.Put(Path.Time_Remaining'Image);
            Text_IO.Put(' '); Put_Path_Set(Path.Traversed); Text_IO.New_Line;

            -- update with better flow
            if not Paths.Contains(Path.Traversed)
               or else Paths(Path.Traversed) < Path.Flow
            then
               Paths.Include(Path.Traversed, Path.Flow);
            end if;

            -- loop through my neighbors
            Find_Good_Routes: for Dest in Valve_Travel_Times(Path.Name).Iterate
            loop

               -- i wish there was a less cumbersome way to loop through maps
               Neighbor := Valve_Travel_Destination_Time_Maps.Key(Dest);
               Time := Valve_Travel_Destination_Time_Maps.Element(Dest);

               if Valve_Flows(Neighbor) > 0
                  and then not Path.Traversed.Contains(Neighbor)
                  and then Valve_Travel_Times(Path.Name)(Neighbor)
                  < Path.Time_Remaining
               then
                  Neighbors.Include(Neighbor);
               end if;

            end loop Find_Good_Routes;

            -- enqueue the worthwhile paths
            Through_Good_Routes: for Neighbor of Neighbors loop

               New_Flow := ( Path.Time_Remaining
                             - Valve_Travel_Times(Path.Name)(Neighbor) - 1)
                  * Valve_Flows(Neighbor) + Path.Flow;
               New_Traversed := Path.Traversed;
               New_Traversed.Include(Neighbor);

               Path_Queue.Enqueue( ( Name => Neighbor,
                                     Flow => New_Flow,
                                     Time_Remaining => Path.Time_Remaining
                                     - Valve_Travel_Times(Path.Name)(Neighbor)
                                     - 1,
                                     Traversed => New_Traversed
                                    )
                                  );

            end loop Through_Good_Routes;

         end;

      end loop Through_Queue;

      Text_IO.Put_Line("scoring" & Paths.Length'Image & " paths");
      for Path_Score_1 in Paths.Iterate loop
         for Path_Score_2 in Paths.Iterate loop

            declare
               Path1: Valve_Name_Sets.Set := Path_Score_Maps.Key(Path_Score_1);
               Flow1: Natural := Path_Score_Maps.Element(Path_Score_1);
               Path2: Valve_Name_Sets.Set := Path_Score_Maps.Key(Path_Score_2);
               Flow2: Natural := Path_Score_Maps.Element(Path_Score_2);
               Num_Scored: Natural := 0;
            begin

               if not Intersects(Path1, Path2) then
                  Result := Natural'Max(Result, Flow1 + Flow2);
               end if;

               Num_Scored := Num_Scored + 1;
               -- for some mysterious reason, this never prints...
               if Num_Scored rem 100 = 0 then
                  Text_IO.Put_Line("compared" & Num_Scored'Image
                                   & " with" & Result'Image
                                   & " the best so far"
                                  );
               end if;

            end;

         end loop;

      end loop;

      return Result;

   end Elephant_Bfs;

begin

   Read_Valves;

   Sort_By_Flow.Sort(All_Valves);
   --  Text_IO.Put_Line(All_Valves'Image);
   Setup_Travel_Times;
   Put_Travel_Times;
   --  Text_IO.Put_Line("you can release at most" & Max_Pressure(Second)'Image);
   Text_IO.Put_Line("you can release at most" & Elephant_Bfs'Image);

end Main;
