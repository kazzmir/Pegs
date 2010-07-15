require File.dirname(__FILE__) + '/../../spec_helper'

ruby_version_is "1.8.7" do
  describe "Integer#odd?" do
    it "returns true when self is an odd number" do
      (-2).odd?.should be_false
      (-1).odd?.should be_true

      0.odd?.should be_false
      1.odd?.should be_true
      2.odd?.should be_false

      bignum_value(0).odd?.should be_false
      bignum_value(1).odd?.should be_true

      (-bignum_value(0)).odd?.should be_false
      (-bignum_value(1)).odd?.should be_true
    end
  end
end
