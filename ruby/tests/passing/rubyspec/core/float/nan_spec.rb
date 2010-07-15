require File.dirname(__FILE__) + '/../../spec_helper'

describe "Float#nan?" do
  it "returns true if self is not a valid IEEE floating-point number" do
    0.0.nan?.should == false
    -1.5.nan?.should == false
    (0.0/0.0).nan?.should == true
  end
end
