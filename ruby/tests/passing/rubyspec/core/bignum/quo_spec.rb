require File.dirname(__FILE__) + '/../../spec_helper'

describe "Bignum#quo" do
  before(:each) do
    @bignum = bignum_value(3)
  end

  it "returns the result of self divided by the given Integer as a Float" do
    @bignum.quo(0xffff_afed.to_f).should be_close(2147493897.54892, TOLERANCE)
    @bignum.quo(0xabcd_effe).should be_close(3199892875.41007, TOLERANCE)
    @bignum.quo(bignum_value).should be_close(1.00000000279397, TOLERANCE)
  end

  conflicts_with :Rational do
    it "does not raise a ZeroDivisionError when the given Integer is 0" do
      @bignum.quo(0).to_s.should == "Infinity"
      (-@bignum).quo(0).to_s.should == "-Infinity"
    end
  end

  it "does not raise a FloatDomainError when the given Integer is 0 and a Float" do
    @bignum.quo(0.0).to_s.should == "Infinity"
    (-@bignum).quo(0.0).to_s.should == "-Infinity"
  end

  conflicts_with :Rational do
    it "raises a TypeError when given a non-Integer" do
      lambda {
        (obj = mock('to_int')).should_not_receive(:to_int)
        @bignum.quo(obj)
      }.should raise_error(TypeError)
      lambda { @bignum.quo("10") }.should raise_error(TypeError)
      lambda { @bignum.quo(:symbol) }.should raise_error(TypeError)
    end
  end
end