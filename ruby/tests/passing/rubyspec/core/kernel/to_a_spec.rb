require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Kernel#to_a" do
  it "is defined on Kernel" do
    Kernel.instance_methods.should include('to_a')
  end
end

describe "Kernel#to_a when the receiver is an Array" do
  it "returns self" do
    array = [1, 2]
    array.to_a.should equal(array)
  end
end

describe "Kernel#to_a when the receiver is not an Array" do
  it "returns an Array containing self" do
    object = "I am not an array"
    object.to_a.should == [object]
  end
end