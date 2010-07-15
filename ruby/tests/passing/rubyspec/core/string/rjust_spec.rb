require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#rjust with length, padding" do
  it "returns a new string of specified length with self right justified and padded with padstr" do
    "hello".rjust(20, '1234').should == "123412341234123hello"

    "".rjust(1, "abcd").should == "a"
    "".rjust(2, "abcd").should == "ab"
    "".rjust(3, "abcd").should == "abc"
    "".rjust(4, "abcd").should == "abcd"
    "".rjust(6, "abcd").should == "abcdab"

    "OK".rjust(3, "abcd").should == "aOK"
    "OK".rjust(4, "abcd").should == "abOK"
    "OK".rjust(6, "abcd").should == "abcdOK"
    "OK".rjust(8, "abcd").should == "abcdabOK"
  end
  
  it "pads with whitespace if no padstr is given" do
    "hello".rjust(20).should == "               hello"
  end

  it "returns self if it's longer than or as long as the specified length" do
    "".rjust(0).should == ""
    "".rjust(-1).should == ""
    "hello".rjust(4).should == "hello"
    "hello".rjust(-1).should == "hello"
    "this".rjust(3).should == "this"
    "radiology".rjust(8, '-').should == "radiology"
  end

  it "taints result when self or padstr is tainted" do
    "x".taint.rjust(4).tainted?.should == true
    "x".taint.rjust(0).tainted?.should == true
    "".taint.rjust(0).tainted?.should == true
    "x".taint.rjust(4, "*").tainted?.should == true
    "x".rjust(4, "*".taint).tainted?.should == true
  end

  it "tries to convert length to an integer using to_int" do
    "^".rjust(3.8, "^_").should == "^_^"
    
    obj = mock('3')
    def obj.to_int() 3 end
      
    "o".rjust(obj, "o_").should == "o_o"
    
    obj = mock('3')
    obj.should_receive(:respond_to?).with(:to_int).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_int).and_return(3)
    "~".rjust(obj, "~_").should == "~_~"
  end
  
  it "raises a TypeError when length can't be converted to an integer" do
    lambda { "hello".rjust("x")       }.should raise_error(TypeError)
    lambda { "hello".rjust("x", "y")  }.should raise_error(TypeError)
    lambda { "hello".rjust([])        }.should raise_error(TypeError)
    lambda { "hello".rjust(mock('x')) }.should raise_error(TypeError)
  end

  it "tries to convert padstr to a string using to_str" do
    padstr = mock('123')
    def padstr.to_str() "123" end
    
    "hello".rjust(10, padstr).should == "12312hello"

    obj = mock('k')
    obj.should_receive(:respond_to?).with(:to_str).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_str).and_return("k")

    "hello".rjust(7, obj).should == "kkhello"
  end

  it "raises a TypeError when padstr can't be converted" do
    lambda { "hello".rjust(20, :sym)      }.should raise_error(TypeError)
    lambda { "hello".rjust(20, ?c)        }.should raise_error(TypeError)
    lambda { "hello".rjust(20, mock('x')) }.should raise_error(TypeError)
  end
  
  it "raises an ArgumentError when padstr is empty" do
    lambda { "hello".rjust(10, '') }.should raise_error(ArgumentError)
  end
  
  it "returns subclass instances when called on subclasses" do
    StringSpecs::MyString.new("").rjust(10).class.should == StringSpecs::MyString
    StringSpecs::MyString.new("foo").rjust(10).class.should == StringSpecs::MyString
    StringSpecs::MyString.new("foo").rjust(10, StringSpecs::MyString.new("x")).class.should == StringSpecs::MyString
    
    "".rjust(10, StringSpecs::MyString.new("x")).class.should == String
    "foo".rjust(10, StringSpecs::MyString.new("x")).class.should == String
  end
end
