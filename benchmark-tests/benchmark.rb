#!/usr/bin/ruby

def cons1 x, y
  [x, y]
end

Conses = []
def cons2 x, y
  Conses << x << y
end

1000000.times {|i| cons1 3, 5}
puts "cons1"

1000000.times {|i| cons2 3, 5}
puts "cons2"
