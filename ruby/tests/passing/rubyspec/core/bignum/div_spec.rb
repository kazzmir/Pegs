require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/shared/divide'

describe "Bignum#div" do
  it_behaves_like(:bignum_divide, :div)

  # Note: #div should always return Integers, not Floats!
  ruby_bug "#", "1.8.6" do
    it "returns a result of integer division of self by a float argument" do
      bignum_value(88).div(0xffff_ffff.to_f).should eql(2147483648)
      bignum_value(88).div(bignum_value(88).to_f).should eql(1)
      bignum_value(88).div(-bignum_value(88).to_f).should eql(-1)
    end
    
    it "raises FloatDomainError if the argument is a float zero" do
      lambda { bignum_value(88).div(0.0) }.should raise_error(FloatDomainError, "Infinity")
      lambda { bignum_value(88).div(-0.0) }.should raise_error(FloatDomainError, "-Infinity")
    end
  end
end
