require File.dirname(__FILE__) + '/../../../spec_helper'
require 'set'

ruby_version_is "1.8.7" do
  describe "SortedSet#hash" do
    it "should be static" do
      SortedSet[].hash.should == SortedSet[].hash
      SortedSet[1, 2, 3].hash.should == SortedSet[1, 2, 3].hash
      SortedSet["a", "b", "c"].hash.should == SortedSet["c", "b", "a"].hash
    
      SortedSet[].hash.should_not == SortedSet[1, 2, 3].hash
      SortedSet[1, 2, 3].hash.should_not == SortedSet["a", "b", "c"].hash
    end
  end
end
