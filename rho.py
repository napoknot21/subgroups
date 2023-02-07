#!/usr/bin/python3
import random
import sys


def pgcd(a, b):
    if b == 0:
        return a
    else:
        return pgcd(b, a % b)


def pollard_rho(n):
    x = random.randint(2, n-1)
    y, k, i, d = x, 2, 1, 1
    while d == 1:
        i = i + 1
        x = (x * x - 1) % n
        d = pgcd(abs(y - x), n)
        if i == k:
            y, k = x, 2*k
    return d

def power(x, y, p):

    # Initialize result
    res = 1

    while (y > 0):

        # If y is odd, multiply x with result
        if ((y & 1) != 0):
            res = res * x

        # y must be even now
        y = y >> 1  # y = y/2
        x = x * x  # Change x to x^2

    return res % p


def compute_ut(n):
    b = bin(n - 1)
    i = len(b) - 1
    t = 0
    while b[i] == '0':
        print(b[i])
        t = t+1
        i = i - 1
    t = 1 if t == 0 else t
    u = b[:len(b) - t - 1] +'1'
    u = int(u, 2)
    print(u)
    return u, t


def mira_witness(a, n):
    if n < 3:
        return False
    if n % 2 == 0:
        return True
    u, t = compute_ut(n)
    x = pow(a, u, n)
    for _ in range(t):
        y = pow(x, 2, n)
        print(y)
        if y == 1:
            return False
        x = y
    return x != 1


def is_prime(n, s=50):
    for i in range(1, s):
        a = random.randint(1, n-1)
        if mira_witness(a, n):
            return False
    return True


def fact(n):
    if is_prime(n):
        return [n]
    if n == 0:
        return []
    l = []
    while n > 1:
        if (n % 2 == 0):
            l.append(2)
            d = 2
        else:
            d = pollard_rho(n)
            l = l + fact(d)
        n = n // d
    return l


if __name__ == '__main__':
    n = int(sys.argv[1])
    print("Calcul pour n = ", n)
    print(fact(n))
