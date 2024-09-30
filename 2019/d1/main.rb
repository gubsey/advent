sumP1 = 0
sumP2 = 0

ARGF.each_line do |e|
     i = Integer(e) / 3 - 2
     sumP1 += i
     while i > 0
          sumP2 += i
          i /= 3
          i -= 2
     end
end

puts 'part 1: %d' %[sumP1]
puts 'part 2: %d' %[sumP2]