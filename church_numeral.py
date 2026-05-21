from functools import reduce, partial
from collections.abc import Callable


def church_numeral(n: int) -> Callable[[Callable], Callable[[int], int]]:
    return lambda f: partial(reduce, (lambda x, g: g(x)), n*(f,))


def add_one(i: int) -> int:
    return i + 1


if __name__ == '__main__':
    print(church_numeral(4)(add_one)(0))
    print(church_numeral(7)(add_one)(0))
    print(church_numeral(2)(add_one)(0))
    print(church_numeral(13)(add_one)(0))
