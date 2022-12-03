-- Advent of Code 2022, Day 3
--
-- John Perry
--
-- Rucksack Reorganization
--
-- part 1: sum priorities for items that appear
-- in each rucksack's two compartments;
-- fortunately, each rucksack has only one common item in each compartment
--
-- part 2: sum priorities for items that appear in each group of 3 rucksacks;
-- fortunately, groups are already grouped,
-- and each group has only one common item

with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

procedure Main is

   -- SECTION
   -- input-related, part 1

   Filename: constant String := "input.txt";
   package ATIO renames Ada.Text_IO;
   Input_File : ATIO.File_Type;

   -- SECTION
   -- evaluating your priorities

   Bad_Item: exception;

   function Priority(C: Character) return Positive is
      (
       case C is
          when 'a' .. 'z' => Character'Pos(C) - Character'Pos('a') + 1,
          when 'A' .. 'Z' => Character'Pos(C) - Character'Pos('A') + 27,
          when others => raise Bad_Item with C'Image
      );

   -- SECTION
   -- part 1

   procedure Prioritize_Items_In_Both_Compartments
   -- find the item that appears in both halves of each rucksack,
   -- sum the priorities of all these items
   --
   -- this was straightforward enough that I didn't bother saving the data

   is
      Priority_Sum: Natural := 0;

   begin

      ATIO.Open(Input_File, ATIO.In_File, Filename);

      loop

         declare
            Rucksack          : String := ATIO.Get_Line(Input_File);
            Len               : Positive := Rucksack'Length;
            -- I rather enjoyed the fact that next two lines just worked
            Left_Compartment  : String := Rucksack(1..Len / 2);
            Right_Compartment : String := Rucksack(Len / 2 + 1..Len);
            Found             : Boolean := False;
            Common_Item       : Character;

         begin

            for Item of Left_Compartment loop

               for Other_Item of Right_Compartment loop
                  if Item = Other_Item then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if Found then
                  Common_Item := Item;
                  exit;
               end if;

            end loop;

            -- there better be a common item!
            Priority_Sum := Priority_Sum + Priority(Common_Item);

         end;

         if ATIO.End_Of_File(Input_File) then exit; end if;

      end loop;

      ATIO.Put_Line("Priority sum is" & Priority_Sum'Image);
      ATIO.Close(Input_File);

   end Prioritize_Items_In_Both_Compartments;

   procedure Prioritize_Items_Common_To_Groups_Of_Three
   -- find the item that appears in each group of three rucksacks,
   -- sum the priorities of all these items
   --
   -- luckily, the rucksacks are already grouped;
   -- I worried it would be more complicated than that
   --
   -- this could be made much more efficient with the use of Hashed_Sets,
   -- which I did in the subsequent procedure

   is
      Priority_Sum: Natural := 0;

   begin

      ATIO.Open(Input_File, ATIO.In_File, Filename);

      loop

         declare
            First: String := ATIO.Get_Line(Input_File);
            Second: String := ATIO.Get_Line(Input_File);
            Third : String := ATIO.Get_Line(Input_File);
            Common_Item: Character;
            Found      : Boolean := False;

         begin

            for Item_1 of First loop
               for Item_2 of Second loop

                  if Item_1 = Item_2 then -- no point continuing if not

                     for Item_3 of Third loop

                        if Item_1 = Item_3 then
                           Common_Item := Item_1;
                           Found := True;
                           exit;
                        end if;

                     end loop;

                     if Found then exit; end if;

                  end if;

               end loop;

               if Found then exit; end if;

            end loop;

            Priority_Sum := Priority_Sum + Priority(Common_Item);

         end;

         if ATIO.End_Of_File(Input_File) then exit; end if;

      end loop;

      ATIO.Put_Line("Priority sum is" & Priority_Sum'Image);
      ATIO.Close(Input_File);

   end Prioritize_Items_Common_To_Groups_Of_Three;

   procedure Alternate_Prioritize_Items_Common_To_Groups_Of_Three
   -- find the item that appears in each group of three rucksacks,
   -- sum the priorities of all these items
   --
   -- luckily, the rucksacks are already grouped;
   -- I worried it would be more complicated than that
   --
   -- this version uses Hashed_Sets

   is
      Priority_Sum: Natural := 0;

      function Hash_Character(C: Character) return Ada.Containers.Hash_Type is
         ( Character'Pos(C) );

      package Item_Sets is new Ada.Containers.Hashed_Sets
         (
          Element_Type => Character,
          Hash         => Hash_Character,
          Equivalent_Elements => "="
         );


   begin

      ATIO.Open(Input_File, ATIO.In_File, Filename);

      loop

         declare
            First: String := ATIO.Get_Line(Input_File);
            Second: String := ATIO.Get_Line(Input_File);
            Third : String := ATIO.Get_Line(Input_File);
            Common_Item: Character;
            First_Set, Second_Set, Third_Set: Item_Sets.Set;

         begin

            -- set up the hashed sets
            for Item_1 of First loop
               First_Set.Include(Item_1);
            end loop;
             for Item_2 of Second loop
               Second_Set.Include(Item_2);
            end loop;
            for Item_3 of Third loop
               Third_Set.Include(Item_3);
            end loop;

            -- much, MUCH simpler search!
            for Item_1 of First_Set loop
               if Second_Set.Contains(Item_1) and Third_Set.Contains(Item_1) then
                  Common_Item := Item_1;
                  exit;
               end if;
            end loop;

            Priority_Sum := Priority_Sum + Priority(Common_Item);

         end;

         if ATIO.End_Of_File(Input_File) then exit; end if;

      end loop;

      ATIO.Put_Line("Alternate priority sum is" & Priority_Sum'Image);
      ATIO.Close(Input_File);

   end Alternate_Prioritize_Items_Common_To_Groups_Of_Three;

begin

   Prioritize_Items_In_Both_Compartments;
   Prioritize_Items_Common_To_Groups_Of_Three;
   Alternate_Prioritize_Items_Common_To_Groups_Of_Three;

end Main;
