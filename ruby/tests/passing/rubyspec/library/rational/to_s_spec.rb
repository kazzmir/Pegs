require File.dirname(__FILE__) + '/../../spec_helper'
require 'rational'

describe "Rational#to_s" do
  it "returns a string representation of self" do
    Rational(1, 1).to_s.should == "1"
    Rational(2, 1).to_s.should == "2"
    Rational(1, 2).to_s.should == "1/2"
    Rational(-1, 3).to_s.should == "-1/3"
    Rational(1, -3).to_s.should == "-1/3"
  end
end
