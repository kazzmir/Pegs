require File.dirname(__FILE__) + '/../../../spec_helper'
require 'set'

describe "SortedSet#to_a" do
  it "returns an array containing elements of self" do
    SortedSet[1, 2, 3].to_a.sort.should == [1, 2, 3]
  end
end
