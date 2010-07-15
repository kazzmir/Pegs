require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

# TODO: Add missing String#[]= specs:
#   String#[range] = obj
#   String#[re] = obj
#   String#[re, idx] = obj
#   String#[str] = obj

describe "String#[]= with index" do
  it "sets the code of the character at idx to char modulo 256" do
    a = "hello"
    a[0] = ?b
    a.should == "bello"
    a[-1] = ?a
    a.should == "bella"
    a[-1] = 0
    a.should == "bell\x00"
    a[-5] = 0
    a.should == "\x00ell\x00"
    
    a = "x"
    a[0] = ?y
    a.should == "y"
    a[-1] = ?z
    a.should == "z"
    
    a[0] = 255
    a[0].should == 255
    a[0] = 256
    a[0].should == 0
    a[0] = 256 * 3 + 42
    a[0].should == 42
    a[0] = -214
    a[0].should == 42
  end
 
  it "raises an IndexError without changing self if idx is outside of self" do
    a = "hello"
    
    lambda { a[20] = ?a }.should raise_error(IndexError)
    a.should == "hello"
    
    lambda { a[-20] = ?a }.should raise_error(IndexError)
    a.should == "hello"
    
    lambda { ""[0] = ?a  }.should raise_error(IndexError)
    lambda { ""[-1] = ?a }.should raise_error(IndexError)
  end

  it "calls to_int on index" do
    str = "hello"
    str[0.5] = ?c
    str.should == "cello"
  
    obj = mock('-1')
    obj.should_receive(:to_int).and_return(-1)
    str[obj] = ?y
    str.should == "celly"
  
    obj = mock('-1')
    obj.should_receive(:respond_to?).with(:to_int).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_int).and_return(-1)
    str[obj] = ?!
    str.should == "cell!"
  end
  
  it "sets the code to char % 256" do
    str = "Hello"
    
    str[0] = ?a + 256 * 3
    str[0].should == ?a
    str[0] = -200
    str[0].should == 56
  end
  
  it "doesn't call to_int on char" do
    obj = mock('x')
    obj.should_not_receive(:to_int)
    lambda { "hi"[0] = obj }.should raise_error(TypeError)
  end
  
  compliant_on :ruby, :jruby do
    it "raises a TypeError when self is frozen" do
      a = "hello"
      a.freeze
    
      lambda { a[0] = ?b }.should raise_error(TypeError)
    end
  end
end

describe "String#[]= with String" do
  it "replaces the char at idx with other_str" do
    a = "hello"
    a[0] = "bam"
    a.should == "bamello"
    a[-2] = ""
    a.should == "bamelo"
  end

  it "taints self if other_str is tainted" do
    a = "hello"
    a[0] = "".taint
    a.tainted?.should == true
    
    a = "hello"
    a[0] = "x".taint
    a.tainted?.should == true
  end

  it "raises an IndexError without changing self if idx is outside of self" do
    str = "hello"

    lambda { str[20] = "bam" }.should raise_error(IndexError)
    str.should == "hello"
    
    lambda { str[-20] = "bam" }.should raise_error(IndexError)
    str.should == "hello"

    lambda { ""[0] = "bam"  }.should raise_error(IndexError)
    lambda { ""[-1] = "bam" }.should raise_error(IndexError)
  end

  it "raises IndexError if the string index doesn't match a position in the string" do
    str = "hello"
    lambda { str['y'] = "bam" }.should raise_error(IndexError)
    str.should == "hello"
  end

  it "raises IndexError if the regexp index doesn't match a position in the string" do
    str = "hello"
    lambda { str[/y/] = "bam" }.should raise_error(IndexError)
    str.should == "hello"
  end

  compliant_on :ruby, :jruby do
    it "raises a TypeError when self is frozen" do
      a = "hello"
      a.freeze
    
      lambda { a[0] = "bam" }.should raise_error(TypeError)
    end
  end

  it "calls to_int on index" do
    str = "hello"
    str[0.5] = "hi "
    str.should == "hi ello"
  
    obj = mock('-1')
    obj.should_receive(:to_int).and_return(-1)
    str[obj] = "!"
    str.should == "hi ell!"
  
    obj = mock('-1')
    obj.should_receive(:respond_to?).with(:to_int).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_int).and_return(-1)
    str[obj] = "e vator"
    str.should == "hi elle vator"
  end
  
  it "tries to convert other_str to a String using to_str" do
    other_str = mock('-test-')
    def other_str.to_str() "-test-" end
    
    a = "abc"
    a[1] = other_str
    a.should == "a-test-c"
    
    obj = mock('ROAR')
    obj.should_receive(:respond_to?).with(:to_str).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_str).and_return("ROAR")

    a = "abc"
    a[1] = obj
    a.should == "aROARc"
  end
  
  it "raises a TypeError if other_str can't be converted to a String" do
    lambda { "test"[1] = :test     }.should raise_error(TypeError)
    lambda { "test"[1] = mock('x') }.should raise_error(TypeError)
    lambda { "test"[1] = nil       }.should raise_error(TypeError)
  end
end

describe "String#[]= with index, count" do
  it "starts at idx and overwrites count characters before inserting the rest of other_str" do
    a = "hello"
    a[0, 2] = "xx"
    a.should == "xxllo"
    a = "hello"
    a[0, 2] = "jello"
    a.should == "jellollo"
  end
 
  it "counts negative idx values from end of the string" do
    a = "hello"
    a[-1, 0] = "bob"
    a.should == "hellbobo"
    a = "hello"
    a[-5, 0] = "bob"
    a.should == "bobhello"
  end
 
  it "overwrites and deletes characters if count is more than the length of other_str" do
    a = "hello"
    a[0, 4] = "x"
    a.should == "xo"
    a = "hello"
    a[0, 5] = "x"
    a.should == "x"
  end
 
  it "deletes characters if other_str is an empty string" do
    a = "hello"
    a[0, 2] = ""
    a.should == "llo"
  end
 
  it "deletes characters up to the maximum length of the existing string" do
    a = "hello"
    a[0, 6] = "x"
    a.should == "x"
    a = "hello"
    a[0, 100] = ""
    a.should == ""
  end
 
  it "appends other_str to the end of the string if idx == the length of the string" do
    a = "hello"
    a[5, 0] = "bob"
    a.should == "hellobob"
  end
  
  it "taints self if other_str is tainted" do
    a = "hello"
    a[0, 0] = "".taint
    a.tainted?.should == true
    
    a = "hello"
    a[1, 4] = "x".taint
    a.tainted?.should == true
  end
 
  it "raises an IndexError if |idx| is greater than the length of the string" do
    lambda { "hello"[6, 0] = "bob"  }.should raise_error(IndexError)
    lambda { "hello"[-6, 0] = "bob" }.should raise_error(IndexError)
  end
 
  it "raises an IndexError if count < 0" do
    lambda { "hello"[0, -1] = "bob" }.should raise_error(IndexError)
    lambda { "hello"[1, -1] = "bob" }.should raise_error(IndexError)
  end
 
  it "raises a TypeError if other_str is a type other than String" do
    lambda { "hello"[0, 2] = nil  }.should raise_error(TypeError)
    lambda { "hello"[0, 2] = :bob }.should raise_error(TypeError)
    lambda { "hello"[0, 2] = 33   }.should raise_error(TypeError)
  end
end
