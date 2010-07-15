require File.dirname(__FILE__) + '/../../spec_helper'

describe "ObjectSpace.each_object" do
  it "calls the block once for each living, nonimmediate object in the Ruby process" do
    class ObjectSpaceSpecEachObject; end
    new_obj = ObjectSpaceSpecEachObject.new

    count = ObjectSpace.each_object(ObjectSpaceSpecEachObject) {}
    count.should == 1
    # this is needed to prevent the new_obj from being GC'd too early
    new_obj.should_not == nil
  end
end
