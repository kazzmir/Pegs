require File.dirname(__FILE__) + '/../../../spec_helper'
require 'complex'

describe "Numeric#im" do
  it "returns a new Complex number with self as the imaginary component" do
    20.im.should == Complex(0, 20)
    (-4.5).im.should == Complex(0, -4.5)
    bignum_value.im.should == Complex(0, bignum_value)
  end
end