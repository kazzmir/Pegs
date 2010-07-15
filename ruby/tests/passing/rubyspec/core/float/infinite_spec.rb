require File.dirname(__FILE__) + '/../../spec_helper'

describe "Float#infinite?" do
  it "returns nil, -1, +1 when self is finite, -Infinity, +Infinity" do
    1.0.infinite?.should == nil
    (1.0/0.0).infinite?.should == 1
    (1.0/-0.0).infinite?.should == -1
  end
end
