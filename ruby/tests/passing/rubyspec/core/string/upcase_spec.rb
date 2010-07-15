require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#upcase" do
  it "returns a copy of self with all lowercase letters upcased" do
    "Hello".upcase.should == "HELLO"
    "hello".upcase.should == "HELLO"
  end
  
  it "is locale insensitive (only replaces a-z)" do
    "äöü".upcase.should == "äöü"

    str = Array.new(256) { |c| c.chr }.join
    expected = Array.new(256) do |i|
      c = i.chr
      c.between?("a", "z") ? c.upcase : c
    end.join
    
    str.upcase.should == expected
  end
  
  it "taints result when self is tainted" do
    "".taint.upcase.tainted?.should == true
    "X".taint.upcase.tainted?.should == true
    "x".taint.upcase.tainted?.should == true
  end
  
  it "returns a subclass instance for subclasses" do
    StringSpecs::MyString.new("fooBAR").upcase.class.should == StringSpecs::MyString
  end
end

describe "String#upcase!" do
  it "modifies self in place" do
    a = "HeLlO"
    a.upcase!.should equal(a)
    a.should == "HELLO"
  end
  
  it "returns nil if no modifications were made" do
    a = "HELLO"
    a.upcase!.should == nil
    a.should == "HELLO"
  end

  compliant_on :ruby, :jruby do
    it "raises a TypeError when self is frozen" do
      lambda { "HeLlo".freeze.upcase! }.should raise_error(TypeError)
      lambda { "HELLO".freeze.upcase! }.should raise_error(TypeError)
    end
  end
end
