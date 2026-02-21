N = 200
ITERATIONS = 50

def next_rand(seed)
  s = seed * 1103515245 + 12345
  s = -s if s < 0
  s % 1000000007
end

def merge(left, right)
  result = []
  i = 0
  j = 0
  while i < left.length && j < right.length
    if left[i] <= right[j]
      result << left[i]
      i += 1
    else
      result << right[j]
      j += 1
    end
  end
  while i < left.length
    result << left[i]
    i += 1
  end
  while j < right.length
    result << right[j]
    j += 1
  end
  result
end

def mergesort(arr)
  return arr.dup if arr.length <= 1
  mid = arr.length / 2
  left = mergesort(arr[0...mid])
  right = mergesort(arr[mid..])
  merge(left, right)
end

def main
  original = []
  seed = 42
  N.times do
    seed = next_rand(seed)
    original << seed % 10000
  end

  first_element = 0
  ITERATIONS.times do
    sorted = mergesort(original)
    first_element = sorted[0]
  end

  puts first_element
end

main
