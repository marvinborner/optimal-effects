def collatz(i, n):
  if n == 1:
    return i
  else:
    if n % 2 == 0:
      return collatz(i + 1, n / 2)
    else:
      return collatz(i + 1, 3 * n + 1)

def main() -> u24:
  return collatz(0, 25)
