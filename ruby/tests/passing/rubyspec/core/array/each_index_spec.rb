require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Array#each_index" do
  it "passes the index of each element to the block" do
    a = []
    x = ['a', 'b', 'c', 'd']
    x.each_index { |i| a << i }.should equal(x)
    a.should == [0, 1, 2, 3]
  end
  
  it "passes the index of each element to the block even if the array is changed during iteration" do
    a = []
    x = [10, 11, 12, 13,]
    x.each_index {|i| a << i; x << x[i]+5 if (x[i]%2).zero? }.should equal(x)
    a.should == [0, 1, 2, 3, 4, 5]
  end
  
  it "passes the index from 0..size even if size changes" do
    a = []
    x = [10, 11, 12, 13, 14]
    x.each_index {|i| a << i; x.pop if (x[i]%2).zero? }.should equal(x)
    a.should == [0, 1, 2]

    a = []
    x = [10, 11, 12, 13, 14]
    x.each_index {|i| a << i; x.shift if (x[i]%2).zero? }.should equal(x)
    a.should == [0, 1, 2]
  end
end
