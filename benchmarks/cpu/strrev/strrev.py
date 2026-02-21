# Build a string of N digits, reverse it 200 times, print length
# Tests: string allocation, character-level operations

def build_string(n):
    return ''.join(str(i % 10) for i in range(n))

def reverse_string(s):
    return s[::-1]

n = 1000
s = build_string(n)
for _ in range(200):
    s = reverse_string(s)
print(len(s))
