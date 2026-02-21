def is_prime(n)
  return false if n < 2
  d = 2
  while d * d <= n
    return false if n % d == 0
    d += 1
  end
  true
end

count = 0
(2..200000).each do |n|
  count += 1 if is_prime(n)
end
puts count
