-- Advent of Code 2022, Day 21
--
-- John Perry
--
-- Monkey Math
--
-- part 1: figure out what number the monkeys are computing
--
-- part 2: the elephants mistranslated:
--    figure out what number *you* must compute so that the "root" monkey
--    gets equality from both monkeys it listens to
--

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;

   type Part_Number is ( First, Second );

   -- SECTION
   -- global types and variables

   type Operation is ( Number, Sum, Diff, Prod, Quot );
   -- part 2 introduces equality, but Diff will suffice (see below... WAY below)

   subtype Monkey_Name is String(1..4);

   type Monkey(Yells: Operation) is record
      case Yells is
         when Number =>
            Value              : Long_Long_Integer;
         when others =>
            First, Second      : Monkey_Name;
            Value1, Value2     : Long_Long_Integer; -- 1st and 2nd's values,
                                                    -- IF INITIALIZED
            Initialized        : Boolean := False;  -- so this is important
      end case;
   end record;

   function "<"(Left, Right: Monkey_Name) return Boolean is
   -- strictly lexicographic
   begin
      for I in Monkey_Name'First .. Monkey_Name'Last loop
         if Left(I) < Right(I) then return True;
         elsif Left(I) > Right(I) then return False;
         end if;
      end loop;
      return False;
   end "<";

   package Monkey_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (
       Key_Type => Monkey_Name,
       Element_Type => Monkey
      );

   Da_Monkees: Monkey_Maps.Map;

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

   Bad_Operation, Impossible: exception;

   procedure Read_Input is
   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop

         declare
            Input_String: String := Text_IO.Get_Line(Input_File);
            Value       : Integer;
            Pos         : Positive := 7;
            Name1,
               Name2,
               Name3       : Monkey_Name;
         begin

            Name1 := Input_String(1..4);

            if Input_String(7) in '0'..'9' then
               -- a value-able monkey
               Get_Integer(Input_String, Value, Pos);
               Da_Monkees.Include
                  (Key      => Name1,
                   New_Item => ( Yells => Number,
                                 Value => Long_Long_Integer(Value)
                                )
                  );

            else
               -- an operation-al monkey

               Name2 := Input_String(7..10);
               Name3 := Input_String(14..17);

               case Input_String(12) is

                  when '+' => Da_Monkees.Include
                        ( Key => Name1,
                          New_Item => ( Yells      => Sum,
                                        First       => Name2,
                                        Second      => Name3,
                                        Value1      => 0,
                                        Value2      => 0,
                                        Initialized => False
                                       )
                         );

                  when '-' => Da_Monkees.Include
                        ( Key => Name1,
                          New_Item => ( Yells      => Diff,
                                        First       => Name2,
                                        Second      => Name3,
                                        Value1      => 0,
                                        Value2      => 0,
                                        Initialized => False
                                       )
                         );

                  when '*' => Da_Monkees.Include
                        ( Key => Name1,
                          New_Item => ( Yells      => Prod,
                                        First       => Name2,
                                        Second      => Name3,
                                        Value1      => 0,
                                        Value2      => 0,
                                        Initialized => False
                                       )
                         );

                  when '/' => Da_Monkees.Include
                        ( Key => Name1,
                          New_Item => ( Yells      => Quot,
                                        First       => Name2,
                                        Second      => Name3,
                                        Value1      => 0,
                                        Value2      => 0,
                                        Initialized => False
                                       )
                         );

                  when others => raise Bad_Operation; -- never happens, yay

               end case;

            end if;

         end;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Input;

   procedure Print_Monkeys is
   -- prints the monkey map
   -- I wrote this to debug but never needed it! :-)

      Op_Chars: array(Operation) of String(1..1) := ( "?", "+", "-", "*", "/" );

   begin

      for C in Da_Monkees.Iterate loop

         Text_IO.Put(Monkey_Maps.Key(C));

         declare M: Monkey := Monkey_Maps.Element(C);
         begin
            case M.Yells is
               when Number =>
                  Text_IO.Put_Line(" yells " & M.Value'Image);
               when others =>
                  Text_IO.Put_Line(" computes "
                                   & M.First & Op_Chars(M.Yells) & M.Second);
            end case;
         end;

      end loop;

   end Print_Monkeys;

   -- SECTION
   -- PART 1

   function Monkey_Value(M: in out Monkey) return Long_Long_Integer is
   -- returns the number that monkey M will yell out

   begin

      if M.Yells = Number then
         return M.Value;

      else

         if not M.Initialized then
            -- recurse to find values!
            M.Value1 := Monkey_Value(Da_Monkees(M.First));
            M.Value2 := Monkey_Value(Da_Monkees(M.Second));
            M.Initialized := True;
         end if;

         case M.Yells is
            when Sum  => return M.Value1 + M.Value2;
            when Diff => return M.Value1 - M.Value2;
            when Prod => return M.Value1 * M.Value2;
            when Quot => return M.Value1 / M.Value2;
            when others => raise Impossible;
         end case;

      end if;

   end Monkey_Value;

   -- SECTION
   -- PART 2

   -- I thought of inverting the map (which is really a tree)
   -- but we only need it once so why complicate things more than I have to...

   function Uses_Humn(Name: Monkey_Name) return Boolean is
   -- indicates whether the named monkey uses to the human's number,
   -- **even if indirectly**

   begin

      if Name = "humn" then
         -- I AM NOT A MONKEY but I use myself
         return True;

      else

         declare
            C: Monkey_Maps.Cursor := Da_Monkees.Find(Name);
            E: Monkey := Monkey_Maps.Element(C);
         begin
            if E.Yells = Number then return False;
            else
               return Uses_Humn(E.First) or else Uses_Humn(E.Second);
            end if;
         end;

      end if;

   end Uses_Humn;

   function Oracle_Value(Name: Monkey_Name; Goal: Long_Long_Integer := 0)
                         return Long_Long_Integer
   is
   -- returns the value that the named monkey must utter
   -- in order to obtain the `Goal` after applying its operation

      M: Monkey := Da_Monkees(Name);

   begin

      if Name = "humn" then
         -- this one's easy :-)
         return Goal;

      elsif M.Yells = Number then
         -- monkeys who yell numbers are rather stubborn
         return M.Value;

      elsif not Uses_Humn(Name) then
         -- monkeys who don't use the human can keep doing their thing
         return (
                 case M.Yells is
                    when Sum  => M.Value1 + M.Value2,
                    when Diff => M.Value1 - M.Value2,
                    when Prod => M.Value1 * M.Value2,
                    when Quot => M.Value1 / M.Value2,
                    when others => raise Impossible
                );

      else
         -- this monkey depends on two monkeys,
         -- one of which depends on humn
         -- find the value of the one that does not depend on humn,
         -- then dive into the other branch, reversing the operations

         declare
            Known_Value_Name: Monkey_Name
               := ( if Uses_Humn(M.First) then M.Second else M.First );
            Unknown_Value_Name: Monkey_Name
               := ( if Known_Value_Name = M.First then M.Second else M.First );
            N: Monkey := Da_Monkees(Known_Value_Name);
            Known_Value     : Long_Long_Integer
               := ( case N.Yells is
                       when Number => N.Value,
                       when Sum    => N.Value1 + N.Value2,
                       when Diff   => N.Value1 - N.Value2,
                       when Prod   => N.Value1 * N.Value2,
                       when Quot   => N.Value1 / N.Value2
                   );
         begin

            -- it only looks long ;-)
            return
               (
                case M.Yells is

                   when Number => raise Impossible,

                   when Sum    =>
                      Oracle_Value(Unknown_Value_Name, Goal - Known_Value),

                   when Diff   => -- tricky, caught me on the first attempt
                      ( if Unknown_Value_Name = M.First then
                              Oracle_Value(Unknown_Value_Name, Goal + Known_Value)
                        else Oracle_Value(Unknown_Value_Name, Known_Value - Goal)
                       ),

                   when Prod   =>
                      Oracle_Value(Unknown_Value_Name, Goal / Known_Value),

                   when Quot   => -- tricky, caught me on the first attempt
                      ( if Unknown_Value_Name = M.First then
                              Oracle_Value(Unknown_Value_Name, Goal * Known_Value)
                        else Oracle_Value(Unknown_Value_Name, Known_Value / Goal)
                       )
               );
         end;

      end if;

   end Oracle_Value;

begin

   Read_Input;

   Text_IO.Put_Line("part 1:" & Monkey_Value(Da_Monkees("root"))'image);

   -- part 2 wants us to introduce a new equality operation
   -- and make the root monkey test whether its contributing monkeys shout out
   -- equal values
   -- no need for all that!
   -- this is equivalent to subtracting the numbers they shout and getting 0,
   -- so rather than teach the root a new operation we just ask it
   -- (very politely of course)
   -- to determine its goal number from the monkey who doesn't depend on humn,
   -- then communicate that to the monkey who does,
   -- and so on down the chain until we come to humn,
   -- who can just parrot the number received

   declare M: Monkey := Da_Monkees("root");
   begin
      Da_Monkees.Include( "root",
                          ( Yells => Diff,
                            First       => M.First,
                            Second      => M.Second,
                            Value1      => M.Value1,
                            Value2      => M.Value2,
                            Initialized => True -- doesn't matter at this stage
                           )
                         );
   end;
   Text_IO.Put_Line("part 2:" & Oracle_Value("root", 0)'image);

end Main;
