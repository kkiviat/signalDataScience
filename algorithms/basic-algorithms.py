import functools
import time

def timeit(func):
  @functools.wraps(func)
  def newfunc(*args, **kwargs):
    startTime = time.time()
    func(*args, **kwargs)
    elapsedTime = time.time() - startTime
    print('function [{}] finished in {} ms'.format(
      func.__name__, int(elapsedTime * 1000)))
  return newfunc

def sum_multiples(factors, bound):
  """
  returns the sum of all integers from 1 to bound
  that are multiples of any of the numbers in factors
  """
  sum = 0
  for i in range(1, bound):
    for factor in factors:
      if i % factor == 0:
        sum += i
        break
  return sum

def sum_even_fibonacci(bound):
  """
  returns the sum of all even fibonacci  numbers
  that are less than bound (starting with 1, 2)
  """
  if bound <= 0:
    print "must have bound > 0"
    return
  if bound == 1:
    return 0
  f1 = 1
  f2 = 2
  sum = 2
  while f2 < bound:
    f = f1 + f2
    f1 = f2
    f2 = f
    if f % 2 == 0:
      sum += f
  return sum

def sieveGenerator(n):
   """
   Sieve of Erastosthenes
   returns list of all prime numbers < n
   """ 
   isComposite = [False]*n # flags corresponding to 0:n-1
   isComposite[0] = isComposite[1] = True
   index = 0

   for (i, composite) in enumerate(isComposite):
     if not composite:
       yield i
       for j in xrange(i**2, n, i):
         isComposite[j] = True


#@timeit
def sieve(n):
  """
  slow
  """
  if n <= 1: return []
  candidates = range(3, n+1, 2) # get odd numbers
  index = 0
  while index < len(candidates):
    p = candidates[index]
    candidates = filter(lambda x: x <= p or x % p != 0, candidates)
    index += 1
  return [2] + candidates

#@timeit
def sieve2(n):
  """
  slower version
  """
  candidates = range(3, n+1, 2)
  isComposite = [False for i in range(len(candidates))]
  index = 0
  while index < len(candidates):
    p = candidates[index]
    # mark all multiples of p
    for i in range(index+1, len(candidates)):
      if not isComposite[i]:
        isComposite[i] = (candidates[i] % p == 0)
        
    # find next prime
    index += 1
    while index < len(isComposite) and isComposite[index]:
      index += 1
  return [2] + [x for x, composite in zip(candidates, isComposite) if not composite]

def sieve3(n):
  """
  slower generator version
  """
  candidates = range(3, n+1, 2)
  isComposite = [False for i in range(len(candidates))]
  index = 0
  yield 2
  while index < len(candidates):
    p = candidates[index]
    # mark all multiples of p
    yield p
    for i in range(index+1, len(candidates)):
      if not isComposite[i]:
        isComposite[i] = (candidates[i] % p == 0)
        
    # find next prime
    index += 1
    while index < len(isComposite) and isComposite[index]:
      index += 1

import math

def is_prime(n):
  """
  returns whether given number is prime
  """
  if n % 2 == 0: return n == 2
  
  for i in range(3, int(math.sqrt(n)) + 1, 2):
    if n % i == 0: return False
  return True

def get_largest_prime_factor(n):
  """
  returns the largest prime factor of n
  """
  if is_prime(n): return n
  for i in range(int(n/2), 1, -1):
    if n % i == 0 and is_prime(i):
      return i

def is_palindrome(s):
  """
  returns whether given string is a palindrome
  """
  midIndex = len(s)/2
  half1 = s[:midIndex]
  if len(s) % 2 != 0: midIndex += 1
  half2 = s[midIndex:]
  return half1 == half2[::-1]
    
def get_largest_palindrome_product(n):
  """
  returns the largest palindrome number that is the
  product of two n-digit numbers
  """
  products = [i*j for i in range(10**(n-1),10**n) for j in range(i,10**n)]
  return max(filter(lambda x: is_palindrome(str(x)), products))

def get_prime_decomposition(n):
  """
  returns dictionary with all prime factors as keys
  and their multiplicity as values
  """
  if is_prime(n): return {n:1}
  prime_decomposition = {}
  for p in sieve(n/2):
    power = 0
    while n % p**(power+1) == 0:
      power += 1
    if power > 0: prime_decomposition[p] = power
  return prime_decomposition

def multiply_prime_decompositions(d1, d2):
  """
  combines the two prime decompositions so that
  the result is the decomposition of the product
  of the numbers represented by the decompositions
  """
  factors = list(set.union(set(d1.keys()), set(d2.keys())))
  d = dict()
  for f in factors:
    d[f] = d1.get(f, 0) + d2.get(f, 0)
  return d

def lcm_prime_decompositions(d1, d2):
  """
  combines the two prime decompositions so that
  the result is the decomposition of the lcm
  of the numbers represented by the decompositions
  """
  factors = list(set.union(set(d1.keys()), set(d2.keys())))
  d = dict()
  for f in factors:
    d[f] = max(d1.get(f, 0), d2.get(f, 0))
  return d

def gcd_prime_decompositions(d1, d2):
  """
  combines the two prime decompositions so that
  the result is the decomposition of the gcd
  of the numbers represented by the decompositions
  """
  factors = list(set.union(set(d1.keys()), set(d2.keys())))
  d = dict()
  for f in factors:
    d[f] = min(d1.get(f, 0), d2.get(f, 0))
  return d

def get_num_from_prime_decomposition(d):
  return reduce(lambda x,y: x*y, [p**k for p, k in zip(d.keys(), d.values())])
  
def get_smallest_number_divisible_by_all(n):
  """
  returns the smallest number that is divisible by
  all the numbers from 1 to n
  """
  d = reduce(lcm_prime_decompositions, map(get_prime_decomposition, range(2, n+1)))
  return get_num_from_prime_decomposition(d)

def sum_square_difference(n):
  """
  returns the difference between the sum of squares of 1:n
  and the square of the sum of 1:n
  """
  sum_of_squares = sum(map(lambda x: x**2, range(1,n+1)))
  square_of_sum = sum(range(1,n+1))**2
  return square_of_sum - sum_of_squares

def get_nth_prime(n):
  i = 1
  x = 3
  while i < n:
    if is_prime(x):
      i += 1
    x += 2
  return x-2

def get_largest_product_in_series(num, window_size):
  """
  returns the largest sum of window_size adjacent digits
  in num
  """
  max_val = 0
  for i in range(0, len(str(num))-window_size+1):
    slice = str(num)[i:i+window_size]
    window_val = reduce(lambda x,y: x*y, (map(int, list(slice))))
    max_val = max(max_val, window_val)
  return max_val

def test_get_largest_product_in_series():
  
  s = '''
  73167176531330624919225119674426574742355349194934
  96983520312774506326239578318016984801869478851843
  85861560789112949495459501737958331952853208805511
  12540698747158523863050715693290963295227443043557
  66896648950445244523161731856403098711121722383113
  62229893423380308135336276614282806444486645238749
  30358907296290491560440772390713810515859307960866
  70172427121883998797908792274921901699720888093776
  65727333001053367881220235421809751254540594752243
  52584907711670556013604839586446706324415722155397
  53697817977846174064955149290862569321978468622482
  83972241375657056057490261407972968652414535100474
  82166370484403199890008895243450658541227588666881
  16427171479924442928230863465674813919123162824586
  17866458359124566529476545682848912883142607690042
  24219022671055626321111109370544217506941658960408
  07198403850962455444362981230987879927244284909188
  84580156166097919133875499200524063689912560717606
  05886116467109405077541002256983155200055935729725
  71636269561882670428252483600823257530420752963450
  '''
  s = filter(lambda x: x != '\n', s)
  get_largest_product_in_series(int(s), 4) # should be 5832
  get_largest_product_in_series(int(s), 13) # should be 23514624000

def find_special_pythagorean_triplet(n):
  """
  returns the product a*b*c where a < b < c are a Pythagorean
  triplet (a^2 + b^2 = c^2) such that a + b + c = n
  there is only one such triplet for n = 1000
  Should return 31875000 for n = 1000
  """
  for a in range(1, int(n/math.sqrt(2))):
    for b in range(a+1, n/2):
      c = n - (a+b)
      if a**2 + b**2 == c**2:
        return a*b*c
  return None

def calc_sum_of_primes(n):
  """
  returns the sum of primes below n
  should give 142913828922 for n = 2,000,000
  """
  return sum(sieveGenerator(n))
