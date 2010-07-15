require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Array#reverse_each" do
  it "traverses array in reverse order and pass each element to block" do
    a = []
    [1, 3, 4, 6].reverse_each { |i| a << i }
    a.should == [6, 4, 3, 1]
  end

  it "properly handles recursive arrays" do
    res = []
    empty = ArraySpecs.empty_recursive_array
    empty.reverse_each { |i| res << i }
    res.should == [empty]

    res = []
    array = ArraySpecs.recursive_array
    array.reverse_each { |i| res << i }
    res.should == [array, array, array, array, array, 3.0, 'two', 1]
  end

# Is this a valid requirement? -rue
compliant_on :r18, :jruby do
  it "does not fail when removing elements from block" do
    ary = [0, 0, 1, 1, 3, 2, 1, :x]
    
    count = 0
    
    ary.reverse_each do |item|
      count += 1
      
      if item == :x then
        ary.slice!(1..-1)
      end
    end
    
    count.should == 2
  end
end
end
