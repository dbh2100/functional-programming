"""This module demonstrates how to implement the Church numeral in Python.

A Church numeral is how lambda calculus implements a number.  It represents
a natural number using functions. The Church numeral for a number n is a
function that takes two arguments: a function f and a value x, and applies
f to x n times.
"""

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
