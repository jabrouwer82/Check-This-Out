#!/bin/ipython
"""
This module describes both a naive and an intellegent algorithm for answering the following problem:
  'Are there more than 500 ways to tile a 2 by 14 grid using 1 by 2 tiles (identical dominos)?'

Both algorithms reduce this problem to tiling a 1 by N grid using either 1 or 2 length tiles.
The naive algorithm computes every possible list of length N comprising of 1, 2, or 3.
  It then filters that list to only those lists which are valid tilings.
The intellegent algorithm uses a recurrence relationship to generate a count all valid tilings.

Running this module directly will print a list breaking down how many tilings there are given a certain number of
  length two tiles (which is equivalent to a pair of horizontal tiles in the original question).
"""

import itertools
from functools import lru_cache
from scipy.special import comb # type: ignore

from typing import List
from typing import Optional
from typing import Tuple

def is_valid(tiling: Tuple[int], twos: Optional[int] = None) -> bool:
  """Validates the given tiling.

  Ensures the tiling only consists of 1, 2, and 3, and that all 2s and 3s are in ordered pairs representing length
  two tiles. If twos is present, validates that only that many length two tiles are present in the tiling.

  Args:
    tiling: A potential tiling.
    twos: The number of expected length 2 tiles in the tiling.

  Returns:
    Whether or not the given tiling is valid.
  """
  if twos is not None and tiling.count(2) != twos:
    return False

  for i, tile in enumerate(tiling):
    if tile == 1:
      pass
    elif tile == 2:
      if i < len(tiling) - 1 and tiling[i+1] == 3:
        pass
      else:
        return False
    elif tile == 3:
      if i > 0 and tiling[i-1] == 2:
        pass
      else:
        return False
    else:
      return False
  return True

@lru_cache(maxsize=None)
def brute_all_tilings(cols: int) -> List[Tuple[int]]:
  """Generates all possible tilings and filters down to only those which are valid.

  Very expensive to compute, but is cached.

  Args:
    cols: The number of columns for the tilings.

  Returns:
    A list of all valid tilings:
      [(1, 2, 3), (1, 1, 1), (2, 3, 1)]
  """
  return list(filter(lambda x: is_valid(x), itertools.product([1, 2, 3], repeat=cols)))

def brute_num_tilings(cols: int) -> int:
  """Returns the number of valid tilings with the given number of columns.

  Uses the brute force generated list of all tilings.

  Args:
    cols: The number of columns for the tilings.

  Returns:
    A count of all valid tilings for the given length.
  """
  tilings = brute_all_tilings(cols)
  return len(tilings)

def brute_partial_num_tilings(twos: int, cols: int) -> int:
  """Counts the number of valid tilings for the given number of columns and two length tiles.

  Uses te brute force gnereated list of all tilings.

  Args:
    twos: The number of two tiles pairs for the tilings.
    cols: The number of columns in the tilings.

  Returns:
    The number of valid tilings for the given number of columns and two lenght tiles.
  """
  return len(list(filter(lambda x: is_valid(x, twos), brute_all_tilings(cols))))

@lru_cache(maxsize=None)
def partial_num_tilings(twos: int, cols: int) -> int:
  """Counts the number of valid tilings for the given number of columns and two length tiles.

  Uses a recurrence relationship to count the tilings, goes not generate them. Very fast, caches simpler results.

  Args:
    twos: The number of two tiles pairs for the tilings.
    cols: The number of columns in the tilings.

  Returns:
    The number of valid tilings for the given number of columns and two lenght tiles.
  """
  if twos <= 1 or cols <= 1:
    return int(comb(cols-1, twos))
  else:
    return (
        partial_num_tilings(twos-1, cols-2) +
        partial_num_tilings(twos, cols-2) +
        partial_num_tilings(twos-1, cols-3)
    )

def num_tilings(cols: int) -> int:
  """Returns the number of valid tilings with the given number of columns.

  Uses the recursing counting solution, very fast.

  Args:
    cols: The number of columns for the tilings.

  Returns:
    A count of all valid tilings for the given length.
  """
  return sum([partial_num_tilings(twos, cols) for twos in range(cols//2+1)])

def main():
  """Main method, prints a ton of partial results for both the brute force and recursive algorithms for validation."""
  green = '\x1b[6;37;42m'
  black = '\x1b[0m'
  for cols in range(1, 15):
    print(f'For a 2x{cols} grid: {num_tilings(cols)}')
    tot = 0
    brute_tot = brute_num_tilings(cols)
    for twos in range(0, cols//2+1):
      brute_partial = brute_partial_num_tilings(twos, cols)
      partial = partial_num_tilings(twos, cols)
      tot += partial
      if partial == brute_partial:
        print(f' {green}Using {twos} length two tiles: {partial}, {brute_partial}{black}')
      else:
        print(f' Using {horizpairs} length two tiles: {partial}, {brute_partial}')
    if tot == brute_tot:
      print(f' {green}For a total of {tot}, {brute_tot}{black}')
    else:
      print(f' For a total of {tot}, {brute_tot}')

if __name__ == '__main__':
  main()

