require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#center with length, padding" do
  it "returns a new string of specified length with self centered and padded with padstr" do
    "one".center(9, '.').should       == "...one..."
    "hello".center(20, '123').should  == "1231231hello12312312"
    "middle".center(13, '-').should   == "---middle----"

    "".center(1, "abcd").should == "a"
    "".center(2, "abcd").should == "aa"
    "".center(3, "abcd").should == "aab"
    "".center(4, "abcd").should == "abab"
    "".center(6, "xy").should == "xyxxyx"
    "".center(11, "12345").should == "12345123451"

    "|".center(2, "abcd").should == "|a"
    "|".center(3, "abcd").should == "a|a"
    "|".center(4, "abcd").should == "a|ab"
    "|".center(5, "abcd").should == "ab|ab"
    "|".center(6, "xy").should == "xy|xyx"
    "|".center(7, "xy").should == "xyx|xyx"
    "|".center(11, "12345").should == "12345|12345"
    "|".center(12, "12345").should == "12345|123451"

    "||".center(3, "abcd").should == "||a"
    "||".center(4, "abcd").should == "a||a"
    "||".center(5, "abcd").should == "a||ab"
    "||".center(6, "abcd").should == "ab||ab"
    "||".center(8, "xy").should == "xyx||xyx"
    "||".center(12, "12345").should == "12345||12345"
    "||".center(13, "12345").should == "12345||123451"
  end
  
  it "pads with whitespace if no padstr is given" do
    "two".center(5).should    == " two "
    "hello".center(20).should == "       hello        "
  end
  
  it "returns self if it's longer than or as long as the specified length" do
    "".center(0).should == ""
    "".center(-1).should == ""
    "hello".center(4).should == "hello"
    "hello".center(-1).should == "hello"
    "this".center(3).should == "this"
    "radiology".center(8, '-').should == "radiology"
  end

  it "taints result when self or padstr is tainted" do
    "x".taint.center(4).tainted?.should == true
    "x".taint.center(0).tainted?.should == true
    "".taint.center(0).tainted?.should == true
    "x".taint.center(4, "*").tainted?.should == true
    "x".center(4, "*".taint).tainted?.should == true
  end
  
  it "tries to convert length to an integer using to_int" do
    "_".center(3.8, "^").should == "^_^"
    
    obj = mock('3')
    def obj.to_int() 3 end
      
    "_".center(obj, "o").should == "o_o"
    
    obj = mock('true')
    obj.should_receive(:respond_to?).with(:to_int).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_int).and_return(3)
    "_".center(obj, "~").should == "~_~"
  end
  
  it "raises a TypeError when length can't be converted to an integer" do
    lambda { "hello".center("x")       }.should raise_error(TypeError)
    lambda { "hello".center("x", "y")  }.should raise_error(TypeError)
    lambda { "hello".center([])        }.should raise_error(TypeError)
    lambda { "hello".center(mock('x')) }.should raise_error(TypeError)
  end
  
  it "tries to convert padstr to a string using to_str" do
    padstr = mock('123')
    def padstr.to_str() "123" end
    
    "hello".center(20, padstr).should == "1231231hello12312312"

    obj = mock('x')
    obj.should_receive(:respond_to?).with(:to_str).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_str).and_return("k")

    "hello".center(7, obj).should == "khellok"
  end
  
  it "raises a TypeError when padstr can't be converted to a string" do
    lambda { "hello".center(20, ?o)        }.should raise_error(TypeError)
    lambda { "hello".center(20, :llo)      }.should raise_error(TypeError)
    lambda { "hello".center(20, mock('x')) }.should raise_error(TypeError)
  end
  
  it "raises an ArgumentError if padstr is empty" do
    lambda { "hello".center(10, "") }.should raise_error(ArgumentError)
    lambda { "hello".center(0, "")  }.should raise_error(ArgumentError)
  end
  
  it "returns subclass instances when called on subclasses" do
    StringSpecs::MyString.new("").center(10).class.should == StringSpecs::MyString
    StringSpecs::MyString.new("foo").center(10).class.should == StringSpecs::MyString
    StringSpecs::MyString.new("foo").center(10, StringSpecs::MyString.new("x")).class.should == StringSpecs::MyString
    
    "".center(10, StringSpecs::MyString.new("x")).class.should == String
    "foo".center(10, StringSpecs::MyString.new("x")).class.should == String
  end
end
