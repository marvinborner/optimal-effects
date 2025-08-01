def fac(n: u24) -> u24:
  bend n, acc=1:
    when n > 1:
      return fork(n - 1, n * acc)
    else:
      return acc

def main() -> u24:
  return fac(10)
