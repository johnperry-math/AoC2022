# AoC2022

[Advent of Code 2022](https://adventofcode.com/2022). **In Ada, no less!**
Because, after 2019 and 2020, I _still_ haven't learned my lesson.

The elves are on an expedition for reindeer food.

## Day 1: Calorie Counting

Determine which elf/elves have the most calories, and report how many calories.
* Part 1 asks for the most calories carried by any one elf.
* Part 2 asks for the calories carried by the three elves with the most calories.

## Day 2: Rock Paper Scissors

An elf gives you a strategy guide for playing rock-paper-scissors.
This strikes you as suspicious, so you check it out.
* Part 1 has you score the strategy when you play it one way.
* Part 2 has you score the strategy when you play it another way.

(Seems odd that the scores were always natural;
given that you suspect the elf, a loss should have a negative score IMHO.)

## Day 3: Rucksack Reorganization

Elves have common items in rucksacks.
* Part 1 asks for the sum of the priorities of items
  common to each rucksack's two compartments.
  Fortunately, each rucksack has only one common item.
* Part 2 asks for the sum of the priorities of items
  common each each group of three rucksacks.
  Fortunately, the groups are already grouped,
  and each group has only one common item.

  I ended up solving this three different ways:
  * In the first, I just searched through the strings.
  * In the second, I used hashed sets of characters to simplify the searching.
    It might be a little quicker, too, but the input is small,
    so I haven't bothered measuring it.
  * In the third, I used an array of booleans indexed by a range of characters
    that includes our desired character set.
    This should be much quicker than the first two approaches,
    though it took a while to remember how to implement this properly.

## Day 4: Camp Cleanup

The elves get to work cleaning up their camp,
but the work assignments have issues.
* Part 1 has you identify which pairs of elves have redundant assignments;
  that is, one elf has an assignment that encompasses the other's.
* Part 2 has you identify which pairs of elves have overlapping assignments.

I solved this first in Ada, and that was relatively quick.
Then I decided to try adding SPARK to it, and that took some time.
Eventually I got it to where correctness is proved, and
one function (`Next_Number`) has interesting pre- and post-conditions.

## Day 5: Supply Stacks

The elves have to load the ships,
but they don't know how the crane will rearrange the crates.
* Part 1 relies on the Crate Mover 9000,
  which can move only one crate at a time.
* Part 2 relies on the Crate Mover 9001,
  which can move multiple crates at a time.

This gave me an opportunity to refresh my memory on Ada's subprogram type;
just define

    type Crane is access procedure (Number, From, To: Positive);

and then you can use the crane you like:

    procedure Crate_Mover_9000(Number, From, To: Positive) is begin ... end;

    procedure Crate_Mover_9001(Number, From, To: Positive) is begin ... end;

    procedure Rearrange_Crates(Use_Crane: Crane) is
    begin
       ...
               Use_Crane(Number_To_Move, From_Stack, To_Stack);
       ...
    end;
