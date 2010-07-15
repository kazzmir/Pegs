require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Array#assoc" do
  it "returns the first array whose 1st item is == obj or nil" do
    s1 = ["colors", "red", "blue", "green"] 
    s2 = [:letters, "a", "b", "c"]
    s3 = [4]
    s4 = [nil, nil]
    a = [s1, s2, s3, s4]
    a.assoc(s1.first).should == s1
    a.assoc(s2.first).should == s2
    a.assoc(s3.first).should == s3
    a.assoc(s4.first).should == s4
    a.assoc(4).should == [4]
    a.assoc("key not in array").should == nil
  end

  it "calls == on first element of each array" do
    key1 = 'it'
    key2 = mock('key2')
    items = [['not it', 1], [ArraySpecs::AssocKey.new, 2], ['na', 3]]

    items.assoc(key1).should == items[1]
    items.assoc(key2).should == nil
  end
  
  it "ignores any non-Array elements" do
    [1, 2, 3].assoc(2).should == nil
    s1 = [4]
    s2 = [5, 4, 3]
    a = ["foo", [], s1, s2, nil, []] 
    a.assoc(s1.first).should == s1
    a.assoc(s2.first).should == s2
  end
end
