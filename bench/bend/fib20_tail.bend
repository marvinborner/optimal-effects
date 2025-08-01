def fib(n: u24) -> u24:
  bend n, a=1, b=0:
    when 0 != n:
      return fork(n - 1, a + b, a)
    else:
      return b

def main() -> u24:
  return fib(20)
