require File.dirname(__FILE__) + '/../../spec_helper'
require 'pathname'

describe "Pathname#root?" do

  it "should return true for root directories" do
    Pathname.new('/').root?.should == true
  end

  it "should return false for empty string" do
    Pathname.new('').root?.should == false
  end

  it "should return false for a top level directory" do
    Pathname.new('/usr').root?.should == false
  end

  it "should return false for a top level with .. appended directory" do
    Pathname.new('/usr/..').root?.should == false
  end

  it "should return false for a directory below top level" do
    Pathname.new('/usr/local/bin/').root?.should == false
  end

end

