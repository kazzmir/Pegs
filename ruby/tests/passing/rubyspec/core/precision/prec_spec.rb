require File.dirname(__FILE__) + '/../../spec_helper'

describe "Float#prec" do
  it "returns the same Float when given the class Float" do
    1.4.prec(Float).should == 1.4
  end

  it "converts the Float to an Integer when given the class Integer" do
    1.4.prec(Integer).should == 1
  end
end

describe "Integer#prec" do
  it "returns the same Integer when given the class Integer" do
    1.prec(Integer).should == 1
  end

  it "converts the Integer to Float when given the class Float" do
    1.prec(Float).should == 1.0
  end
end

