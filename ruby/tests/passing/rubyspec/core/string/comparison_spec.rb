require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#<=> with String" do
  it "compares individual characters based on their ascii value" do
    ascii_order = Array.new(256) { |x| x.chr }
    sort_order = ascii_order.sort
    sort_order.should == ascii_order
  end
  
  it "returns -1 when self is less than other" do
    ("this" <=> "those").should == -1
  end

  it "returns 0 when self is equal to other" do
    ("yep" <=> "yep").should == 0
  end

  it "returns 1 when self is greater than other" do
    ("yoddle" <=> "griddle").should == 1
  end
  
  it "considers string that comes lexicographically first to be less if strings have same size" do
    ("aba" <=> "abc").should == -1
    ("abc" <=> "aba").should == 1
  end

  it "doesn't consider shorter string to be less if longer string starts with shorter one" do
    ("abc" <=> "abcd").should == -1
    ("abcd" <=> "abc").should == 1
  end

  it "compares shorter string with corresponding number of first chars of longer string" do
    ("abx" <=> "abcd").should == 1
    ("abcd" <=> "abx").should == -1
  end
  
  it "ignores subclass differences" do
    a = "hello"
    b = StringSpecs::MyString.new("hello")
    
    (a <=> b).should == 0
    (b <=> a).should == 0
  end
end

# Note: This is inconsistent with Array#<=> which calls to_str instead of
# just using it as an indicator.
describe "String#<=>" do
  it "returns nil if its argument does not respond to to_str" do
    ("abc" <=> 1).should == nil
    ("abc" <=> :abc).should == nil
    ("abc" <=> mock('x')).should == nil
  end
  
  it "returns nil if its argument does not respond to <=>" do
    obj = mock('x')
    def obj.to_str() "" end
    
    ("abc" <=> obj).should == nil
  end
  
  it "compares its argument and self by calling <=> on obj and turning the result around if obj responds to to_str" do
    obj = mock('x')
    def obj.to_str() "" end
    def obj.<=>(arg) 1  end
    
    ("abc" <=> obj).should == -1
    ("xyz" <=> obj).should == -1
    
    obj = mock('x')
    other = "abc"
    obj.should_receive(:respond_to?).with(:to_str).and_return(true)
    obj.should_receive(:respond_to?).with(:<=>).and_return(true)
    obj.should_receive(:method_missing).with(:<=>, other).and_return(-1)
    (other <=> obj).should == +1
  end
end
