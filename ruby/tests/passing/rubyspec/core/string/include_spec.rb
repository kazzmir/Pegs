require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#include? with String" do
  it "returns true if self contains other_str" do
    "hello".include?("lo").should == true
    "hello".include?("ol").should == false
  end
  
  it "ignores subclass differences" do
    "hello".include?(StringSpecs::MyString.new("lo")).should == true
    StringSpecs::MyString.new("hello").include?("lo").should == true
    StringSpecs::MyString.new("hello").include?(StringSpecs::MyString.new("lo")).should == true
  end
  
  it "tries to convert other to string using to_str" do
    other = mock('lo')
    def other.to_str() "lo" end
    "hello".include?(other).should == true

    obj = mock('o')
    obj.should_receive(:respond_to?).with(:to_str).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_str).and_return("o")
    "hello".include?(obj).should == true
  end
  
  it "raises a TypeError if other can't be converted to string" do
    lambda { "hello".include?(:lo)       }.should raise_error(TypeError)
    lambda { "hello".include?(mock('x')) }.should raise_error(TypeError)
  end
end

describe "String#include? with Fixnum" do
  it "returns true if self contains the given char" do
    "hello".include?(?h).should == true
    "hello".include?(?z).should == false
    "hello".include?(0).should == false
  end
  
  it "uses fixnum % 256" do
    "hello".include?(?h + 256 * 3).should == true
  end
  
  it "doesn't try to convert fixnum to an Integer using to_int" do
    obj = mock('x')
    obj.should_not_receive(:to_int)
    lambda { "hello".include?(obj) }.should raise_error(TypeError)
  end
end
