# Build a string of N digits, reverse it 200 times, print length
# Tests: string allocation, character-level operations

def build_string(n)
  (0...n).map { |i| (i % 10).to_s }.join
end

def reverse_string(s)
  s.reverse
end

n = 1000
s = build_string(n)
200.times { s = reverse_string(s) }
puts s.length
