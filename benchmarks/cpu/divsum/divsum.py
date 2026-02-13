def divsum(n):
    total = 0
    for i in range(1, n + 1):
        s = 0
        for j in range(1, i + 1):
            if i % j == 0:
                s += j
        total += s
    return total

print(divsum(10000))
