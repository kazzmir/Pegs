require File.dirname(__FILE__) + '/../../spec_helper'
require 'strscan'

describe "StringScanner#[]" do
  before :each do
    @s = StringScanner.new("Fri Jun 13 2008 22:43")
  end

  it "returns the n-th subgroup in the most recent match" do
    @s.scan(/(\w+) (\w+) (\d+) /)
    @s[0].should == "Fri Jun 13 "
    @s[1].should == "Fri"
    @s[2].should == "Jun"
    @s[3].should == "13"
    @s[-3].should == "Fri"
    @s[-2].should == "Jun"
    @s[-1].should == "13"
  end

  it "returns nil if index is outside of self" do
    @s.scan(/(\w+) (\w+) (\d+) /)
    @s[5].should == nil
    @s[-5].should == nil
  end

  it "calls to_int on the given index" do
    @s.scan(/(\w+) (\w+) (\d+) /)
    @s[0.5].should == "Fri Jun 13 "
  end

  it "raises a TypeError if the given index is nil" do
    @s.scan(/(\w+) (\w+) (\d+) /)
    lambda { @s[nil]}.should raise_error(TypeError)
  end

  it "raises a TypeError when a String is as argument" do
    @s.scan(/(\w+) (\w+) (\d+) /)
    lambda { @s["Fri"]}.should raise_error(TypeError)
  end

  it "raises a TypeError when a Range is as argument" do
    @s.scan(/(\w+) (\w+) (\d+) /)
    lambda { @s[0..2]}.should raise_error(TypeError)
  end
end
