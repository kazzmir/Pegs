require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#ljust with length, padding" do
  it "returns a new string of specified length with self left justified and padded with padstr" do
    "hello".ljust(20, '1234').should == "hello123412341234123"

    "".ljust(1, "abcd").should == "a"
    "".ljust(2, "abcd").should == "ab"
    "".ljust(3, "abcd").should == "abc"
    "".ljust(4, "abcd").should == "abcd"
    "".ljust(6, "abcd").should == "abcdab"

    "OK".ljust(3, "abcd").should == "OKa"
    "OK".ljust(4, "abcd").should == "OKab"
    "OK".ljust(6, "abcd").should == "OKabcd"
    "OK".ljust(8, "abcd").should == "OKabcdab"
  end
  
  it "pads with whitespace if no padstr is given" do
    "hello".ljust(20).should == "hello               "
  end

  it "returns self if it's longer than or as long as the specified length" do
    "".ljust(0).should == ""
    "".ljust(-1).should == ""
    "hello".ljust(4).should == "hello"
    "hello".ljust(-1).should == "hello"
    "this".ljust(3).should == "this"
    "radiology".ljust(8, '-').should == "radiology"
  end

  it "taints result when self or padstr is tainted" do
    "x".taint.ljust(4).tainted?.should == true
    "x".taint.ljust(0).tainted?.should == true
    "".taint.ljust(0).tainted?.should == true
    "x".taint.ljust(4, "*").tainted?.should == true
    "x".ljust(4, "*".taint).tainted?.should == true
  end

  it "tries to convert length to an integer using to_int" do
    "^".ljust(3.8, "_^").should == "^_^"
    
    obj = mock('3')
    def obj.to_int() 3 end
      
    "o".ljust(obj, "_o").should == "o_o"
    
    obj = mock('3')
    obj.should_receive(:respond_to?).with(:to_int).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_int).and_return(3)
    "~".ljust(obj, "_~").should == "~_~"
  end
  
  it "raises a TypeError when length can't be converted to an integer" do
    lambda { "hello".ljust("x")       }.should raise_error(TypeError)
    lambda { "hello".ljust("x", "y")  }.should raise_error(TypeError)
    lambda { "hello".ljust([])        }.should raise_error(TypeError)
    lambda { "hello".ljust(mock('x')) }.should raise_error(TypeError)
  end

  it "tries to convert padstr to a string using to_str" do
    padstr = mock('123')
    def padstr.to_str() "123" end
    
    "hello".ljust(10, padstr).should == "hello12312"

    obj = mock('k')
    obj.should_receive(:respond_to?).with(:to_str).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_str).and_return("k")

    "hello".ljust(7, obj).should == "hellokk"
  end

  it "raises a TypeError when padstr can't be converted" do
    lambda { "hello".ljust(20, :sym)      }.should raise_error(TypeError)
    lambda { "hello".ljust(20, ?c)        }.should raise_error(TypeError)
    lambda { "hello".ljust(20, mock('x')) }.should raise_error(TypeError)
  end
  
  it "raises an ArgumentError when padstr is empty" do
    lambda { "hello".ljust(10, '') }.should raise_error(ArgumentError)
  end
  
  it "returns subclass instances when called on subclasses" do
    StringSpecs::MyString.new("").ljust(10).class.should == StringSpecs::MyString
    StringSpecs::MyString.new("foo").ljust(10).class.should == StringSpecs::MyString
    StringSpecs::MyString.new("foo").ljust(10, StringSpecs::MyString.new("x")).class.should == StringSpecs::MyString
    
    "".ljust(10, StringSpecs::MyString.new("x")).class.should == String
    "foo".ljust(10, StringSpecs::MyString.new("x")).class.should == String
  end
end
