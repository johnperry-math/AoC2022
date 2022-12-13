-- Advent of Code 2022, Day 13
--
-- John Perry
--
-- Distress Signal
--
-- part 1:
--
-- part 2:
--

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is

   package Text_IO renames Ada.Text_IO;
   package Natural_IO is new Ada.Text_IO.Integer_IO ( Num => Natural );

   Doing_Example: constant Boolean := False;

   -- SECTION
   -- global types and variables

   type Packet;
   type Packet_Ptr is access all Packet; -- recursive type :-(

   package Packet_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Ada.Containers.Count_Type,
        Element_Type => Packet_Ptr
       );

   type Packet_Type is ( Number, List );
   type Packet (Kind: Packet_Type) is record
      case Kind is
         when Number => Value: Natural;
         when List => Sub_Packet: Packet_Vectors.Vector;
      end case;
   end record;

   procedure Put_Packet(P: Packet) is
   -- prints `P` to standard out
   -- this proved invaluable during debugging
   begin
      if P.Kind = Number then
         Natural_IO.Put(P.Value, 0);
      else
         Text_IO.Put("[ ");
         for Q of P.Sub_Packet loop
            Put_Packet(Q.all);
            Text_IO.Put(", ");
         end loop;
         Text_IO.Put("]");
      end if;
   end Put_Packet;

   function "<"(Left, Right: Packet) return Boolean is
   -- returns true according to the rules given in the puzzle,
   -- and described **briefly** below

      use all type Ada.Containers.Count_Type;

   begin

      if Left.Kind = Number and Right.Kind = Number then
         -- both numbers; compare values
         return Left.Value < Right.Value;

      elsif Left.Kind = List and Right.Kind = List then
         -- compare sub-packet lengths

         if Left.Sub_Packet.Is_Empty then
            -- left is empty; result depends on whether right is empty
            return not Right.Sub_Packet.Is_Empty;

         elsif Right.Sub_Packet.Is_Empty then
            -- left is not empty, but right is
            return False;

         else
            -- left is not empty
            -- compare the first value, then the second value, and so on

            for I in Left.Sub_Packet.First_Index .. Left.Sub_Packet.Last_Index
            loop

               if Right.Sub_Packet.Last_Index < I then
                  -- right ran out of elements first
                  return False;

               elsif Left.Sub_Packet(I).all < Right.Sub_Packet(I).all then
                  -- left ran out of elements first
                  return True;

               elsif Right.Sub_Packet(I).all < Left.Sub_Packet(I).all then
                  -- whoops, right's ith element is smaller than left's
                  return False;

               end if;

               -- if we get here, the two elements must have been equal;
               -- we have to keep looking
            end loop;

            -- the lists were equal up to the end of `Left`
            -- right had better be longer!
            return Left.Sub_Packet.Length < Right.Sub_Packet.Length;

         end if;

      elsif Left.Kind = Number then
         -- right must be a list; make left into a list and compare
         declare
            New_List: Packet(Kind => List);
            New_Item: Packet_Ptr := new Packet'( Kind => Number, Value => Left.Value );
         begin
            New_List.Sub_Packet.Append(New_Item);
            return New_List < Right;
         end;

      else
         -- left must be a list; make right into a list and compare
         declare
            New_List: Packet(Kind => List);
            New_Item: Packet_Ptr := new Packet'(Kind => Number, Value => Right.Value );
         begin
            New_List.Sub_Packet.Append(New_Item);
            return Left < New_List;
         end;

      end if;

   end "<";

   -- SECTION
   -- I/O

   Filename: constant String
      := ( if Doing_Example then "example.txt" else "input.txt" );

   Input_File: Text_IO.File_Type;

   Unexpected_Character: exception;

   procedure Parse_Element
      (S: String; Pos: in out Positive; Into: in out Packet_Vectors.Vector);
   -- forward declaration

   procedure Parse_List
      (S  : String;
       Pos: in out Positive;
       Into: in out Packet_Vectors.Vector
      )
   is
   -- parses S as if it a list starts in position `Pos`
   -- S(Pos) should be '['
   -- at termination, S(pos - 1) = ']'; that is,
   -- you can start parsing at S(pos)

      Start: Positive := Pos;
      Sub_Packet: Packet_Ptr := new Packet(Kind => List);

   begin

      Pos := Pos + 1; -- start at first element
      while Pos < S'Length and then S(Pos) /= ']' loop
         -- parse each element of the list
         Parse_Element(S, Pos, Sub_Packet.Sub_Packet);
      end loop;
      Pos := Pos + 1; -- move past ']'

      Into.Append(Sub_Packet);

   end Parse_List;

   procedure Parse_Number
      (S  : String;
       Pos: in out Positive;
       Into: in out Packet_Vectors.Vector
      )
   is
   -- parses S as if a number started at S(pos)
   -- S(pos) should be a digit
   -- at termination, S(pos - 1) is the last digit; that is,
   -- you can start parsing at s(pos)

      Sub_Packet: Packet_Ptr := new Packet(Kind => Number);

   begin

      Natural_IO.Get(S(Pos .. S'Last), Sub_Packet.Value, Pos);
      Into.Append(Sub_Packet);
      Pos := Pos + 1;

   end Parse_Number;

   procedure Parse_Element
      (S  : String;
       Pos: in out Positive;
       Into: in out Packet_Vectors.Vector
      )
   is
   -- parses the element of S that starts at position `Pos`
   -- this should be either a bracket or a digit
   -- anything else will raise an exception

      Start: Positive := Pos;

   begin

      loop

         if S(Pos) = '[' then
            Parse_List(S, Pos, Into);
         elsif S(Pos) in '0' .. '9' then
            Parse_Number(S, Pos, Into);
         else
            raise Unexpected_Character with S(Pos)'Image & "at" & Pos'Image;
         end if;

         -- is there another element in the list?
         if Pos < S'Length and then S(Pos) = ',' then
            Pos := Pos + 1;
         else
            exit;
         end if;

      end loop;

   end Parse_Element;

   type Packet_Pair is record
      First, Second: Packet(Kind => List);
   end record;
   -- the input for part 1 is given as pairs of packets
   -- and we need to determine whether they are in the correct order

   package Packet_Pair_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Positive,
        Element_Type => Packet_Pair
       );

   All_Packets: Packet_Pair_Vectors.Vector;

   procedure Read_Packets is
   -- reads the input from disk
   -- pairs of packets are expected,
   -- separated by empty lines
   begin

      Text_IO.Open(Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File(Input_File) loop

         -- read the two packets
         declare
            Pos: Positive := 1;
            First_Packet_String: String := Text_IO.Get_Line(Input_File);
            Second_Packet_String: String := Text_IO.Get_Line(Input_File);
            First_Packet, Second_Packet: Packet(Kind => List);
         begin

            Parse_Element(First_Packet_String, Pos, First_Packet.Sub_Packet);
            Pos := 1;
            Parse_Element(Second_Packet_String, Pos, Second_Packet.Sub_Packet);
            All_Packets.Append
               ( ( First => First_Packet, Second => Second_Packet ) );

         end;

         -- read the empty line
         if not Text_IO.End_Of_File(Input_File) then
            declare
               Empty_String: String := Text_IO.Get_Line(Input_File);
            begin
               null;
            end;
         end if;

      end loop;

      Text_IO.Close(Input_File);

   end Read_Packets;

   function Num_In_Right_Order return Natural is
   -- counts how many pairs of packets that are in the correct order

      Result: Natural := 0;

   begin

      for I in 1 .. All_Packets.Last_Index loop
         if All_Packets(I).First < All_Packets(I).Second then
            Result := Result + I;
         end if;
      end loop;

      return Result;

   end Num_In_Right_Order;

   function Decoder_Key return Natural is
   -- puts all the packets into a list, adding the two guard packets 2 and 6,
   -- sorts them, then returns the product of the indices of the guard packets
   --
   -- technically the guard packets are supposed to be [[2]] and [[6]],
   -- but when you compare a number to a list, it creates a list for the number,
   -- so this works out (at least for me)
   --
   -- I supposed I should turn them into proper lists one day

      List_Of_Packets: Packet_Vectors.Vector;
      Two_Guard      : Packet_Ptr := new Packet'( Kind => Number, Value => 2 );
      Six_Guard      : Packet_Ptr := new Packet'( Kind => Number, Value => 6 );
      Decoder_Key    : Natural := 1;

      function "<"(Left, Right: Packet_Ptr) return Boolean is
         ( Left.all < Right.all );

      package Packet_Ptr_Sorter is new Packet_Vectors.Generic_Sorting
         ("<" => "<");

   begin

      for Packet_Pair of All_Packets loop
         declare
            First : Packet_Ptr := new Packet'
               ( Kind => List, Sub_Packet => Packet_Pair.First.Sub_Packet);
            Second: Packet_Ptr := new Packet'
               ( Kind => List, Sub_Packet => Packet_Pair.Second.Sub_Packet);
         begin
            List_Of_Packets.Append(First);
            List_Of_Packets.Append(Second);
         end;
      end loop;

      List_Of_Packets.Append(Two_Guard);
      List_Of_Packets.Append(Six_Guard);

      Packet_Ptr_Sorter.Sort(List_Of_Packets);

      for I in List_Of_Packets.First_Index .. List_Of_Packets.Last_Index loop
         if List_Of_Packets(I) = Two_Guard
            or else List_Of_Packets(I) = Six_Guard
         then
            Decoder_Key := Decoder_Key * (Natural(I) + 1);
         end if;
      end loop;

      return Decoder_Key;

   end Decoder_Key;

begin

   Read_Packets;

   Text_IO.Put_Line("the sum of indices in correct order is"
                    & Num_In_Right_Order'Image
                   );

   Text_IO.Put_Line("the decoder key is" & Decoder_Key'Image);

end Main;
