require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Enumerable#sort_by" do
  it "returns an array of elements ordered by the result of block" do
    a = EnumerableSpecs::Numerous.new("once", "upon", "a", "time")
    a.sort_by { |i| i[0] }.should == ["a", "once", "time", "upon"]
  end

  it "sorts the object by the given attribute" do
    a = EnumerableSpecs::SortByDummy.new("fooo")
    b = EnumerableSpecs::SortByDummy.new("bar")

    ar = [a, b].sort_by { |d| d.s }
    ar.should == [b, a]
  end
end
