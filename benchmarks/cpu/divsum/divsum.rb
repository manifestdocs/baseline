def divsum(n)
  total = 0
  (1..n).each do |i|
    s = 0
    (1..i).each do |j|
      s += j if i % j == 0
    end
    total += s
  end
  total
end

puts divsum(10000)
