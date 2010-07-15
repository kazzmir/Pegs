require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/shared/divide'

describe "Bignum#/" do
  it_behaves_like(:bignum_divide, :/)

  it "returns self divided by float" do
    (bignum_value(88) / 0xffff_ffff.to_f).should be_close(2147483648.5, TOLERANCE)
  end

  it "does NOT raise ZeroDivisionError if other is zero and is a Float" do
    (bignum_value / 0.0).to_s.should == 'Infinity'
    (bignum_value / -0.0).to_s.should == '-Infinity'
  end
end
