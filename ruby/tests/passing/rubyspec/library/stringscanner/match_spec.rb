require File.dirname(__FILE__) + '/../../spec_helper'
require 'strscan'

describe "StringScanner#match?" do
  before :each do
    @s = StringScanner.new("This is a test")
  end

  it "returns the length of the match and the scan pointer is not advanced" do
    @s.match?(/\w+/).should == 4
    @s.match?(/\w+/).should == 4
    @s.pos.should == 0
  end

  it "returns nil if there's no match" do
    @s.match?(/\d+/).should == nil
    @s.match?(/\s+/).should == nil
  end
end
