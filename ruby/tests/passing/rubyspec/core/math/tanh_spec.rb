require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Math.tanh" do
  it "returns a float" do
    Math.tanh(0.5).class.should == Float
  end
  
  it "returns the hyperbolic tangent of the argument" do
    Math.tanh(0.0).should == 0.0
    Math.tanh(-0.0).should == -0.0
    Math.tanh(1.0/0.0).should == 1.0
    Math.tanh(1.0/-0.0).should == -1.0
    Math.tanh(2.5).should be_close(0.98661429815143, TOLERANCE)
    Math.tanh(-4.892).should be_close(-0.999887314427707, TOLERANCE)
  end
  
  it "raises an ArgumentError if the argument cannot be coerced with Float()" do
    lambda { Math.tanh("test") }.should raise_error(ArgumentError)
  end

  it "raises a TypeError if the argument is nil" do
    lambda { Math.tanh(nil) }.should raise_error(TypeError)
  end    
  
  it "accepts any argument that can be coerced with Float()" do
    Math.tanh(MathSpecs::Float.new).should be_close(0.761594155955765, TOLERANCE)
  end
end

describe "Math#tanh" do
  it "is accessible as a private instance method" do
    IncludesMath.new.send(:tanh, 5.21).should be_close(0.99994034202065, TOLERANCE)
  end
end
