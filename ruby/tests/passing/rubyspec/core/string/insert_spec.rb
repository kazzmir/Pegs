require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#insert with index, other" do
  it "inserts other before the character at the given index" do
    "abcd".insert(0, 'X').should == "Xabcd"
    "abcd".insert(3, 'X').should == "abcXd"
    "abcd".insert(4, 'X').should == "abcdX"
  end
  
  it "modifies self in place" do
    a = "abcd"
    a.insert(4, 'X').should == "abcdX"
    a.should == "abcdX"
  end
  
  it "inserts after the given character on an negative count" do
    "abcd".insert(-5, 'X').should == "Xabcd"
    "abcd".insert(-3, 'X').should == "abXcd"
    "abcd".insert(-1, 'X').should == "abcdX"
  end
  
  it "raises an IndexError if the index is beyond string" do
    lambda { "abcd".insert(5, 'X')  }.should raise_error(IndexError)
    lambda { "abcd".insert(-6, 'X') }.should raise_error(IndexError)
  end
  
  it "converts index to an integer using to_int" do
    other = mock('-3')
    def other.to_int() -3 end
    "abcd".insert(other, "XYZ").should == "abXYZcd"

    obj = mock('-3')
    obj.should_receive(:respond_to?).with(:to_int).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_int).and_return(-3)
    "abcd".insert(obj, "XYZ").should == "abXYZcd"
  end
  
  it "converts other to a string using to_str" do
    other = mock('XYZ')
    def other.to_str() "XYZ" end
    "abcd".insert(-3, other).should == "abXYZcd"

    obj = mock('X')
    obj.should_receive(:respond_to?).with(:to_str).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_str).and_return("X")
    "abcd".insert(-3, obj).should == "abXcd"
  end

  it "taints self if string to insert is tainted" do
    str = "abcd"
    str.insert(0, "T".taint).tainted?.should == true

    str = "abcd"
    other = mock('T')
    def other.to_str() "T".taint end
    str.insert(0, other).tainted?.should == true
  end
  
  it "raises a TypeError if other can't be converted to string" do
    lambda { "abcd".insert(-6, ?e)        }.should raise_error(TypeError)
    lambda { "abcd".insert(-6, :sym)      }.should raise_error(TypeError)
    lambda { "abcd".insert(-6, mock('x')) }.should raise_error(TypeError)
  end
  
  compliant_on :ruby, :jruby do
    it "raises a TypeError if self is frozen" do
      str = "abcd".freeze
      lambda { str.insert(4, '')  }.should raise_error(TypeError)
      lambda { str.insert(4, 'X') }.should raise_error(TypeError)
    end
  end
end
