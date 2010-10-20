#lang ruby

x = lambda{|y| [5] * y }
puts x.call(4)
