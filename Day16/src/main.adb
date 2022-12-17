-- Advent of Code 2022, Day 16
--
-- John Perry
--
-- Proboscidea Volcanium
--
-- part 1:
--
-- part 2:
--

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

   package Positive_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Positive,
        Element_Type => Positive
       );

   subtype Name_Character is Character range 'A' .. 'Z';

   type Valve_Name is record
      Name1, Name2: Name_Character;
   end record;

   function "<"(Left, Right: Valve_Name) return Boolean is
      ( Left.Name1 < Right.Name1
        or else ( Left.Name1 = Right.Name1 and then Left.Name2 < Right.Name2 )
       );

   package Valve_Flow_Maps is new Ada.Containers.Ordered_Maps
      ( Key_Type => Valve_Name,
        Element_Type => Natural
       );

   package Valve_Name_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Ada.Containers.Count_Type,
        Element_Type => Valve_Name
       );

   use all type Ada.Containers.Count_Type;

   function "="(Left, Right: Valve_Name_Vectors.Vector) return Boolean is
      (
       Left.Length = Right.Length and then
          ( for all I in 1 .. Left.Length => Left(I) = Right(I) )
      );

   package Valve_Name_Sets is new Ada.Containers.Ordered_Sets
      ( Element_Type => Valve_Name );

   package Valve_Tunnel_Maps is new Ada.Containers.Ordered_Maps
      (
       Key_Type => Valve_Name,
       Element_Type => Valve_Name_Vectors.Vector
      );

   All_Valves: Valve_Name_Vectors.Vector;
   Valve_Flows: Valve_Flow_Maps.Map;
   Valve_Tunnels: Valve_Tunnel_Maps.Map;

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

   Valve_Travel_Times: Valve_Travel_Maps.Map;

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

   procedure Put_Travel_Times(Times: Valve_Travel_Destination_Time_Maps.Map) is
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
   begin
      for Name of All_Valves loop
         Text_IO.Put(Name.Name1); Text_IO.Put(Name.Name2); Text_IO.Put(": ");
         Put_Travel_Times(Valve_Travel_Times(Name));
      end loop;
   end Put_Travel_Times;

   procedure Find_Min_Travel_Times(From: Valve_Name) is

      type Valve_And_Time is record
         Name: Valve_Name;
         Time : Natural;
      end record;
      package Queued_Valve_Interfaces
      is new Ada.Containers.Synchronized_Queue_Interfaces
         ( Element_Type => Valve_And_Time );
      package Queued_Valves_Queues
      is new Ada.Containers.Unbounded_Synchronized_Queues
         ( Queue_Interfaces => Queued_Valve_Interfaces );
      Queued_Valves: Queued_Valves_Queues.Queue;

      Explored: Valve_Name_Sets.Set;
      New_Map: Valve_Travel_Destination_Time_Maps.Map;

   begin
      --  Text_IO.Put("finding travel times for ");
      --  Text_IO.Put(From.Name1); Text_IO.Put(From.Name2);
      --  Text_IO.New_Line;

      -- prime the queue
      Explored.Include(From);
      Valve_Travel_Times.Include(From, New_Map);
      Valve_Travel_Times(From).Include(From, 0);
      for Valve of Valve_Tunnels(From) loop
         Queued_Valves.Enqueue( ( Valve, 1 ) );
         Valve_Travel_Times(From).Include(Valve, 1);
         Explored.Include(Valve);
         --  Text_IO.Put("enqueuing ");
         --  Text_IO.Put(Valve.Name1); Text_IO.Put(Valve.Name2);
         --  Text_IO.Put(" 1");
         --  Text_IO.New_Line;
      end loop;
      --  Text_IO.New_Line;

      while Queued_Valves.Current_Use > 0 loop
         --  Text_IO.Put_Line("current state:");
         --  Text_IO.Put("   ");
         --  Put_Travel_Times(Valve_Travel_Times(From));
         declare
            Valve: Valve_And_Time;
         begin
            Queued_Valves.Dequeue(Valve);
            --  Text_IO.Put("dequeuing ");
            --  Text_IO.Put(Valve.Name.Name1); Text_IO.Put(Valve.Name.Name2);
            --  Text_IO.Put_Line(Valve.Time'Image);

            for Destination of Valve_Tunnels(Valve.Name) loop

               --  Text_IO.Put("   checking ");
               --  Text_IO.Put(Destination.Name1); Text_IO.Put(Destination.Name2);
               --  Text_IO.New_Line;
               --  Text_IO.Put_Line("      "& Explored.Contains(Destination)'Image);
               --  Text_IO.Put_Line("      "& Valve_Travel_Times.Contains(Destination)'Image);
               if not Explored.Contains(Destination) then
                  --  Text_IO.Put("   enqueuing ");
                  --  Text_IO.Put(Destination.Name1); Text_IO.Put(Destination.Name2);
                  --  Text_IO.Put(Natural'Image(Valve.Time + 1));
                  --  Text_IO.New_Line;
                  Explored.Include(Destination);
                  Valve_Travel_Times(From).Include(Destination, Valve.Time + 1);
                  Queued_Valves.Enqueue( ( Destination, Valve.Time + 1 ) );
               end if;
            end loop;
            --  Text_IO.New_Line;
         end;
      end loop;
   end Find_Min_Travel_Times;

   procedure Setup_Travel_Times is
   begin
      for Valve of All_Valves loop
         Find_Min_Travel_Times(Valve);
      end loop;
   end Setup_Travel_Times;

   function Expected_Value(From, Dest: Valve_Name; Current_Time: Natural)
                           return Natural
   is
      Travel_Time: Natural := Valve_Travel_Times(From)(Dest);
      Flow: Natural := Valve_Flows(Dest);
   begin
      return ( Natural'Max(0, ( 30 - ( Current_Time + Travel_Time) ) * Flow ) );
   end;

   procedure Put_Path(Path: Valve_Name_Vectors.Vector) is
   begin
      for Valve of Path loop
         Text_IO.Put(Valve.Name1); Text_IO.Put(Valve.Name2); Text_IO.Put(" ");
      end loop;
      Text_IO.New_Line;
   end Put_Path;

   function Max_Benefit
      (From: Valve_Name;
       Current_Path: Valve_Name_Vectors.Vector;
       Current_Time: Natural
      )
       return Natural
   is
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

   function Flow_From
      (From        : Valve_Name;
       Current_Time: Natural;
       Current_Flow: Natural;
       Currently_On: Valve_Name_Vectors.Vector
      )
       return Natural
   is
      Result: Natural := Current_Flow;
      Last_Flow: Natural := 0;
   begin
      Text_IO.Put("time" & Current_Time'Image & ": ");
      Put_Path(Currently_On);
      --  Put_Travel_Times(Valve_Travel_Times(From)); Text_IO.New_Line;
      for Dest of All_Valves loop
         if Valve_Flows(Dest) > 0
            and then not Currently_On.Contains(Dest)
         then
            declare
               Next_On: Valve_Name_Vectors.Vector := Currently_On;
            begin
               --  Text_IO.Put("Appending "); Text_IO.Put(Dest.Name1); Text_IO.Put(Dest.Name2);
               --  Text_IO.Put(" to ");
               --  Put_Path(Next_On);
               --  Text_IO.Put_Line("with expected value" & Expected_Value(From, Dest, Current_Time)'image);
               --  Text_IO.Put_Line("for total value" & Natural'Image(Current_Flow + Expected_Value(From, Dest, Current_Time)));
               --  Text_IO.New_Line;
               if Max_Benefit(Dest, Currently_On, Current_Time) > Last_Flow then
                  Next_On.Append(Dest);
                  Result := Natural'Max
                     (Result,
                      Flow_From
                         (Dest,
                          Current_Time + Valve_Travel_Times(From)(Dest) + 1,
                          Current_Flow + Expected_Value(From, Dest, Current_Time),
                          Next_On
                         )
                     );
                  Last_Flow := Result - Current_Flow;
                  --  Text_IO.Put("result of ");
                  --  Put_Path(Next_On);
                  --  Text_IO.Put_Line(Result'Image);
                  --  Text_IO.New_Line;
                  --  Text_IO.Put(Natural'Image(Current_Flow + Expected_Value(From, Dest, Current_Time)));
                  --  Text_IO.New_Line;
               --  else
               --     Text_IO.Put_Line("skipping because only expect"
               --                      & Max_Benefit(Dest, Currently_On, Current_Time)'image
               --                      & " compared to"
               --                      & Last_Flow'Image
               --                     );
               end if;
            end;
         end if;
      end loop;
      --  Text_IO.Put_Line("returning" & Result'Image);
      return Result;
   end Flow_From;

   type Both_Flows is record
      H_Flow, E_Flow: Natural := 0;
   end record;

   function Flow_With_Elephant_From
      (From, Elephant_From        : Valve_Name;
       Current_Time, Elephant_Time: Natural;
       Current_Flow, Elephant_Flow: Natural;
       Currently_On, Elephant_On  : Valve_Name_Vectors.Vector
      )
       return Both_Flows
   is
      Result, Temp: Both_Flows;
   begin
      Result := ( Current_Flow, Elephant_Flow );
      for Dest of All_Valves loop
         if Valve_Flows(Dest) > 0
            and then not Currently_On.Contains(Dest)
            and then not Elephant_On.Contains(Dest)
         then
            declare
               Next_On: Valve_Name_Vectors.Vector;
            begin
               if Current_Time <= Elephant_Time
                  and then Current_Time + Valve_Travel_Times(From)(Dest) + 1 <= 30
               then
                  --  Put_Travel_Times(Valve_Travel_Times(From));
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
               elsif Elephant_Time + Valve_Travel_Times(Elephant_From)(Dest) + 1 <= 30
               then
                  Next_On := Elephant_On;
                  Next_On.Append(Dest);
                  --  Put_Travel_Times(Valve_Travel_Times(Elephant_From));
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
                  --  Last_Flow := Result - Current_Flow;
                  --  Text_IO.Put("result of ");
                  --  Put_Path(Next_On);
                  --  Text_IO.Put_Line(Result'Image);
                  --  Text_IO.New_Line;
                  --  Text_IO.Put(Natural'Image(Current_Flow + Expected_Value(From, Dest, Current_Time)));
                  --  Text_IO.New_Line;
                  --  else
                  --     Text_IO.Put_Line("skipping because only expect"
                  --                      & Max_Benefit(Dest, Currently_On, Current_Time)'image
                  --                      & " compared to"
                  --                      & Last_Flow'Image
                  --                     );
               end if;
            end;
         end if;
      end loop;
      return Result;
   end Flow_With_Elephant_From;

   function Max_Pressure(Part: Part_Number) return Natural is
      Position                 : Valve_Name := ( 'A', 'A' );
      Currently_On, Elephant_On: Valve_Name_Vectors.Vector;
      Part_Two_Result: Both_Flows;
   begin
      Currently_On.Append(Position);
      Elephant_On.Append(Position);
      if Part = First then return Flow_From(Position, 1, 0, Currently_On);
      else Part_Two_Result := Flow_With_Elephant_From
            (Position, Position, 5, 5, 0, 0, Currently_On, Elephant_On);
         return Part_Two_Result.H_Flow + Part_Two_Result.E_Flow;
      end if;
   end Max_Pressure;

   function Greater_Flow(Left, Right: Valve_Name) return Boolean is
      ( Valve_Flows(Left) > Valve_Flows(Right) );

   package Sort_By_Flow is new Valve_Name_Vectors.Generic_Sorting
      ( "<" => Greater_Flow );

begin

   Read_Valves;

   Sort_By_Flow.Sort(All_Valves);
   --  Text_IO.Put_Line(All_Valves'Image);
   Setup_Travel_Times;
   Put_Travel_Times;
   Text_IO.Put_Line("you can release at most" & Max_Pressure(Second)'Image);

end Main;
