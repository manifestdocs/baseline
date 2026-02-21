N = 200
ITERATIONS = 50


def next_rand(seed):
    s = seed * 1103515245 + 12345
    if s < 0:
        s = -s
    s = s % 1000000007
    return s


def merge(left, right):
    result = []
    i = 0
    j = 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1
    while i < len(left):
        result.append(left[i])
        i += 1
    while j < len(right):
        result.append(right[j])
        j += 1
    return result


def mergesort(arr):
    if len(arr) <= 1:
        return arr[:]
    mid = len(arr) // 2
    left = mergesort(arr[:mid])
    right = mergesort(arr[mid:])
    return merge(left, right)


def main():
    original = []
    seed = 42
    for _ in range(N):
        seed = next_rand(seed)
        original.append(seed % 10000)

    first_element = 0
    for _ in range(ITERATIONS):
        sorted_arr = mergesort(original)
        first_element = sorted_arr[0]

    print(first_element)


if __name__ == "__main__":
    main()
