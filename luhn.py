#!/usr/bin/env python3
"""
Provies functions for checking if numbers have valid luhn checksums, and for generating luhn checksums.

Also contians some unittests and code to measure the performance differences betwen two luhn algorithms.
"""
import os
import sys
import unittest

from pandas import DataFrame
from timeit import repeat

GREEN = '\x1b[6;30;42m'
RED = '\x1b[6;37;41m'
NORMAL = '\x1b[0m'

def luhn(num):
  """Checks if the given number has a valid luhn checksum."""
  str_num = str(num)
  return len(str_num) == 0 or luhn_sum(str_num) % 10 == 0

def luhn2(num):
  """Checks if the given number has a valid luhn checksum.

  Generates the expected checksum for the number and verifies it matches the actual checksum.
  """
  str_num = str(num)
  return len(str_num) == 0 or luhn_checkdigit(str_num[:-1]) == int(str_num[-1])

def luhn_checkdigit(num):
  """Produces a luhn checkdigit for the given number."""
  sum_digit = str(luhn_sum(str(num) + '0'))[-1]
  return 0 if sum_digit == '0' else 10 - int(sum_digit)

def luhn_sum(num):
  """Sums the digits in the number via luhns algorithm."""
  return sum(luhnize(digit, ind) for ind, digit in enumerate(digits_rtl(num)))

def luhn_sum2(num):
  """Uses a less efficient iterator than luhn_sum."""
  return sum(luhnize(digit, ind) for ind, digit in enumerate(digits_rtl_2(num)))

def luhn_sum3(num):
  """Uses a less efficient iterator than luhn_sum."""
  return sum(luhnize(digit, ind) for ind, digit in enumerate(digits_rtl_3(num)))

def digits_rtl(num):
  """Creates a generator that iterates over the digits of the number from right to left."""
  val = int(num)
  while val:
    yield val % 10
    val //= 10

def digits_rtl_2(num):
  """Less efficient that digits_rtl, has to convert to and from strings."""
  conv = str if type(num) == str else repr
  return map(int, conv(num)[::-1])

def digits_rtl_3(num):
  """Less efficient than digits_rtl, has to convert to and from strings."""
  conv = str if type(num) == str else repr
  zero = ord('0')
  return map(lambda x: ord(x) - zero, conv(num)[::-1])

def luhnize(digit, index):
  """Performs the appropriate calulations for luhn's mod 10 algorithm

  Double every second digit and subreact 9 to make it a single digit, leave other digits alone.
  """
  if is_even(index):
    return digit
  else:
    if digit < 5:
      return digit * 2
    else:
      return digit * 2 - 9

def is_even(num):
  return num % 2 == 0

LUHN_FUNCS = [luhn, luhn2]
LUHN_SUM_FUNCS = [luhn_sum, luhn_sum2, luhn_sum3]
INVALID_NUMBERS = [
  4,
  27,
  '0002',
  79927398710,
  79927398711,
  79927398712,
  79927398714,
  79927398715,
  79927398716,
  79927398717,
  79927398718,
  79927398719
]
VALID_NUMBERS = [
  0,
  18,
  356,
  '00026',
  79927398713,
  # The following are test numbers from card networks.
  4716482418701311,
  4304172248660336,
  4929302920514886613,
  5542085555437198,
  5146437401032089,
  2720995452994060,
  378922420478595,
  370310762628969,
  372737477069311,
  6011978074253841,
  6011024634906865,
  6011428102789841912,
  3533725857975546,
  3537092484233617,
  3538104137775383269,
  5538906569551003,
  5565692653692541,
  5501779495991830,
  30533265245815,
  30152552791603,
  30129245964037,
  36651591968217,
  36361904367327,
  36950048438232,
  6759071057311566,
  5038425671185373,
  5893761000576054,
  4508613163305179,
  4026630816074856,
  4175007002891287,
  6395495239224670,
  6390517435452758,
  6381693101945929,
]

class LuhnTests(unittest.TestCase):
  def test_is_even(self):
    even_nums = [-4, 0, 2, 2**64, 1432523045198]
    for num in even_nums:
      self.assertTrue(is_even(num))

  def test_is_not_even(self):
    odd_nums = [-3, 1, 20398457, sys.maxsize]
    for num in odd_nums:
      self.assertFalse(is_even(num))

  def test_invalid_luhn(self):
    for func in LUHN_FUNCS:
      for num in INVALID_NUMBERS:
        self.assertFalse(func(num), f'{func.__name__}({num})')

  def test_valid_luhn(self):
    for func in LUHN_FUNCS:
      for num in VALID_NUMBERS:
        self.assertTrue(func(num), f'{func.__name__}({num})')

  # It would probably be wise to split this into several unit tests that check individual parts of the math.
  def test_luhnize(self):
    cases = [
      ((0, 1), 0),
      ((0, 0), 0),
      ((1, 5), 2),
      ((1, 10), 1),
      ((4, 3), 8),
      ((4, 2), 4),
      ((5, 9), 1),
      ((5, 4), 5),
      ((8, 1), 7),
      ((8, 8), 8),
    ]
    for input, output in cases:
      self.assertEqual(luhnize(*input), output, f'luhnize{input}')

  def test_luhn_sum(self):
    cases = [
      (1, 1),
      (11, 3),
      (45, 13),
      (0, 0),
      (5555, 12),
      (1234567890, 43),
      (123456789, 47),
      ('0000000002', 2),
      (79927398710, 67),
    ]
    for func in LUHN_SUM_FUNCS:
      for input, output in cases:
        self.assertEqual(luhn_sum(input), output, f'{func.__name__}({input})')

  def test_luhn_checkdigit(self):
    cases = map(lambda num: (str(num)[:-1], int(str(num)[-1])), VALID_NUMBERS)
    for input, output in cases:
      self.assertEqual(luhn_checkdigit(input), output, f'''luhn_checkdigit('{input}')''')

def overprint(*args, **kwargs):
  term_width = os.get_terminal_size()[0]
  print(' '.join(map(str, args)).ljust(term_width, ' ')[:term_width], **kwargs, end='\r')

def timeit_setup(func_name):
  return f'from __main__ import {func_name}'

def timeit_func(func, value):
  return f'{func.__name__}({repr(value)})'

def time_tests():
  """Runs the luhn alternatives through timeit over various imputs to measure performance."""
  print()
  luhn_results = list()
  luhn_sum_results = list()
  try:
    valid_nums = list([0, 3538104137775383269])
    invalid_nums = list([1, 3538104137775383262])
    for func in LUHN_FUNCS:
      setup = timeit_setup(func.__name__)
      # Enumerate the two lists so the invalid numbers are 0 (False) and the valid ones are 1 (True)
      for is_valid, nums in enumerate([invalid_nums, valid_nums]):
        for num in nums:
          times = sorted(repeat(timeit_func(func, num), setup, repeat=10, number=10000))
          luhn_results.append({
            'func': func.__name__,
            'input': str(num),
            'result': bool(is_valid),
            'time': times[0],
          })
          overprint(func.__name__, num, times)

    to_sum = ['0', 0, '3538104137775383269', 3538104137775383269]
    for func in LUHN_SUM_FUNCS:
      setup = timeit_setup(func.__name__)
      for num in to_sum:
        times = sorted(repeat(timeit_func(func, num), setup, repeat=10, number=20000))
        luhn_sum_results.append({
          'func': func.__name__,
          'input': str(num),
          'time': times[0],
          'type': type(num).__name__,
        })
        overprint(func.__name__, num, times)
  except KeyboardInterrupt:
    pass
  overprint()
  print('Luhn time results:')
  luhn_df = DataFrame(luhn_results, columns=['func', 'time', 'input', 'result'])
  print(luhn_df.sort_values(['input', 'func']).to_string(index=False))
  print()
  print('Luhn sum time results:')
  luhn_sum_df = DataFrame(luhn_sum_results, columns=['func', 'time', 'input', 'type'])
  print(luhn_sum_df.sort_values(['input', 'func']).to_string(index=False))

def test():
  """Runs unit tests."""
  unittest.main(exit=False, argv=sys.argv[0][0:1])

def main(arg):
  if luhn(arg):
    print(f'{GREEN} ✔ {arg} {NORMAL}')
  else:
    print(f'{RED} ✗ {arg} {NORMAL}')

if __name__ == '__main__':
  for arg in sys.argv[1:]:
    if 'test' == arg:
      test()
    elif 'time' == arg:
      time_tests()
    else:
      main(arg)
