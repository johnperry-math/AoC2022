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

## Day 6: Tuning Trouble

The elves need to communicate, but one of their devices is broken.

1. Write a subroutine that detects the beginning-of-message marker,
   and outputs the character index where it ends.
2. Write a subroutine that detects the beginning-of-message marker,
   and outputs the character index where it ends.

One can basically adapt the subroutine for part (1) to part (2).

Since this was relatively straightforward, I adapted it to SPARK,
but I didn't quite work out how to guarantee a postcondition for `Find_Marker`.

## Day 7: No Space Left On Device

The device is still broken. It needs an update, but... well, see the title.
You have a command history of a minimal listing of the file system hierarchy.

1. Identify the sums of the folders that are smaller than 100_000.
2. Identify the smallest folder that is large enough
   to free enough space on the device that you can install the update.

I possibly overengineered this one, but I also got really held up on Part 1
because
* I misread the question, and then
* I neglected to update all the folders; in particular,
  the one(s) that contain the last files in the history.

Once I finished Part 1, Part 2 was a snap.

## Day 8: Treetop Tree House

The elves want to build a tree house.

1. They want it to be hidden, so we have to count how many trees are visible.
   (In my case, this turned out to be roughly 18%.)
2. They want it to have a nice scenic view, defined as the product
   of the number of trees visible in each cardinal direction.

This could be converted to SPARK, but I don't want to stay up late tonight.

## Day 9: Rope Bridge

You're walking on a rope bridge when it does exactly what you'd expect.
The tail(s) follow the head according to some fairly sensible rules.
Determine how many unique positions the tail reaches when the rope has:

1. 2 knots;
2. 10 knots.

Originally I wrote two separate functions,
then condensed them into one function that solves both parts.

## Day 10: Cathode-Ray Tube

Separate from the party, you end up in the water. (Thanks, former rope bridge!)
Your communication device has video issues and you have to replace it.
First, you have to figure out how it works.

1. Determine the signal strength of a sequence of instructions.
2. Determine the message these instructions should be drawing to the CPU.

The solution in the repository is a bit different
from the original, quick 'n dirty one.
This one uses `Ada.Text_IO.Enumeration_IO` to simplify the reading
and processing of instructions.
I also corrected a few bugs that by sheer luck didn't affect the final solution.
For instance, my original solution misplaced the CRT update,
but by some miracle the only practical consequence was that
it neglected to update the top-left pixel.

## Day 11: Monkey in the Middle

Some monkeys have grabbed your stuff, and are tossing items to each other.
How they behave depends on how your interest in the item changes
when they play with it.
After a certain number of rounds,
we can calculate the **level of monkey business**,
which is the product of the two largest numbers of items a monkey inspects.

1. Determine this number after twenty rounds.
   Your relief that the monkeys don't damage the items
   scales the value by 1/3.
2. Determine the number after 10,000 rounds.
   Your relief that the monkeys don't damage the items _is not rescaled_ by 1/3;
   "you'll need to find another way to keep your worry levels manageable."
   :laughing: A bit of number theory saves the day.

The input was fairly small, so rather that write a parser
I put the values directly into the records.
Rather amazingly, I didn't make any typos!

## Day 12: Hill Climbing Algorithm

1. Find the length of the shortest path from Start to End.
2. Find the length of the shortest path from any ground-level point to End.

I wasted entirely too much time trying to make this work with a depth-first
algorithm.

## Day 13: Distress Signal

You try to contact the elves, but your communication device intercepts a signal.
Unfortunately, the packets are out of order.

1. Find the sum of the indices of the packets in the correct order.
2. Add the guard packets `[[2]]` and `[[6]]`, sort the packets,
   and determine the decoder key, which is the product of
   the guard packets' indices.

Premature optimization was the root of all evil once again,
but once I worked out Part 1, Part 2 was pretty much a snap.

## Day 14: Regolith Reservoir

In a cavern that is filling up with sand.

1. Count the number of particles that fall
   before they start falling into the void.
2. Add a ledge. Count the number of particles that fall
   before they pile up to the source.

Pretty straightforward for once.

## Day 15: Beacon Exclusion Zone

Send out sensors to try and detect the distress signal's location.
They do, but none of the beacons you pick up is the one you want.

1. Determine how many positions on row 2_000_000 cannot contain a beacon.
2. Determine the one unique position in the 4_000_000-square box
   cornered at the origin that no sensor can detect.

Several things amaze me about Part 2.

1. It can be solved! That's some amazing planning
   to ensure there's only one position there that no sensor can detect.
2. The solution I devised is about as fast as the solution to Part 1,
   maybe faster. That's because I used a `HashedSet` in Part 1,
   but there's no need to do that in Part 2.
   This makes me suspect there's a way to do Part 1 much faster.
3. I came up with a working solution to Part 2 on the first try.
   (Modulo typos, of course.)

## Day 16: Proboscidea Volcanium

1. Determine the route that most increases pressure.
2. Do it with a volcano.

My solution to part 1 took several tens of minutes.
My solution to part 2 is ongoing after several hours.
I don't quite understand why, as I read what others are doing,
and I _think_ I'm doing the same thing, but they finish in seconds
or even fractions of a second, while here I am... waiting... waiting...

Not a fan of this one.

### UPDATE

Having a much more efficient solution to part 2 using breadth-first search.
Basically, the next time I think a depth-first search should work,
I should knock my head with something to set it straight. Seriously.

I had thought of BFS, actually, but I dismissed it out of hand
as having too many possible solutions.
What I didn't realize, and learned only from looking at a Python solution,
is that an adaptation of BFS will work:
* Keep track of the valves turned on and their maximum flow,
  _not_ the precise order you turned them on.
* I had tried caching solutions for depth-first search, but various issues
  kept cropping up. I'm still not sure what I was doing wrong before,
  but it may have been related to the fact that this time I took the remainder
  with `Ada.Containers.Hash_Type'Last`.

## Day 17: Pyroclastic Flow

1. Count the height of a tower of falling rocks after one million turns.
2. Repeat, but for one trillion turns.

It's always a pleasure when I can put my mathematics degree to use.

## Day 18: Boiling Boulders

1. Count the number of exposed surfaces of cooled, coagulated drops of lava.
2. Repeat, excluding interior pockets of air.

This was a welcome relief from the last two excruciating days,
in part because I was smart enough to:
* perform a breadth-first search when determine which points are interior; and
* start that search from the _exterior_,
  determining the inside points only indirectly.

I thought about doing it in SPARK, but I didn't want to implement a custom queue
for the breadth-first search.

## Day 19: Not Enough Minerals

1. Determine the quality level for each blueprint
   to build robots to mine minerals within 24 minutes.
2. The same, but only the first three blueprints now, within 32 minutes.

Breadth-first search works well here with the right optimization,
and I had the right idea to use BFS, but not the right optimization.
The puzzle is nice in principle, but the execution disappointed me.
The example had two parts, but each took longer to solve than most parts
of the actual input! It was also too complex to follow along when debugging.

## Day 20: Grove Positioning System

1. Sort the values of a circular list according to their values.
2. Repeat after multiplying the values by some ludicrously large number,
   then sorting _10_ times instead of 1.

I had one very good idea, one good idea,
and a few bad ideas, including one very bad idea that held me up a long time.
* The very good idea was to use modular arithmetic,
  so as to minimize the motion:
  no point in shifting a number around the list however many times.
* The good idea was to ... well, I forgot.
* One bad idea was to swap the value with the value at its final position,
  which completely forgets the shift.
* The very bad idea was to think that my list had to end each shift
  with the same starting value as what the website showed.
  I didn't think about the fact that, if it's a circular list,
  all that matters is that the values are in the correct order.
  This is especially true since you get your score
  by counting from 0's position, and 0 is unique,
  so it doesn't really matter which element is first.
