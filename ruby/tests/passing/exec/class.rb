#lang ruby

class Foo
    def initialize
    end

    def q
        puts "hello from foo"
    end

    def bar(x)
        2 + x
    end
end

Foo.new.q
puts "hi"
h = Foo.new
puts h.bar(3)
