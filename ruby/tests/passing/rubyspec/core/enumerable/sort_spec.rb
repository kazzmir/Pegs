require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Enumerable#sort" do
  it "sorts by the natural order as defined by <=> " do
    EnumerableSpecs::Numerous.new.sort.should == [1, 2, 3, 4, 5, 6]
    sorted = EnumerableSpecs::ComparesByVowelCount.wrap("a" * 1, "a" * 2, "a"*3, "a"*4, "a"*5)
    EnumerableSpecs::Numerous.new(sorted[2],sorted[0],sorted[1],sorted[3],sorted[4]).sort.should == sorted
  end

  it "yields elements to the provided block" do
    EnumerableSpecs::Numerous.new.sort { |a, b| b <=> a }.should == [6, 5, 4, 3, 2, 1]
    EnumerableSpecs::Numerous.new(2,0,1,3,4).sort { |n, m| -(n <=> m) }.should == [4,3,2,1,0]
  end

  it "sort should throw a NoMethodError if elements do not define <=>" do
    lambda {
      EnumerableSpecs::Numerous.new(Object.new, Object.new, Object.new).sort
    }.should raise_error(NoMethodError)
  end

  it "sorts enumerables that contain nils" do
    arr = EnumerableSpecs::Numerous.new(nil, true, nil, false, nil, true, nil, false, nil)
    arr.sort { |a, b|
      x = a ? -1 : a.nil? ? 0 : 1
      y = b ? -1 : b.nil? ? 0 : 1
      x <=> y
    }.should == [true, true, nil, nil, nil, nil, nil, false, false]
  end
end

