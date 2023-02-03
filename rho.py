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


def compute_ut(n):
    b = bin(n - 1)
    i = len(b) - 1
    t = 0
    while b[i] == '0':
        t = t+1
        i = i - 1
    t = 1 if t == 0 else t
    u = b[:len(b) - t - 1] + '1'
    return int(u, 2), t


def mira_witness(a, n):
    if n < 3:
        return False
    if n % 2 == 0:
        return True
    u, t = compute_ut(n)
    x = pow(a, u, n)
    for _ in range(t):
        y = pow(x, 2, n)
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
    print(fact(int(n)))
