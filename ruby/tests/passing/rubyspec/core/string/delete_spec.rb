require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#delete" do
  it "returns a new string with the chars from the intersection of sets removed" do
    s = "hello"
    s.delete("lo").should == "he"
    s.should == "hello"
    
    "hello".delete("l", "lo").should == "heo"
    
    "hell yeah".delete("").should == "hell yeah"
  end
  
  it "raises an ArgumentError when given no arguments" do
    lambda { "hell yeah".delete }.should raise_error(ArgumentError)
  end

  it "negates sets starting with ^" do
    "hello".delete("aeiou", "^e").should == "hell"
    "hello".delete("^leh").should == "hell"
    "hello".delete("^o").should == "o"
    "hello".delete("^").should == "hello"
    "^_^".delete("^^").should == "^^"
    "oa^_^o".delete("a^").should == "o_o"
  end

  it "deletes all chars in a sequence" do
    "hello".delete("\x00-\xFF").should == ""
    "hello".delete("ej-m").should == "ho"
    "hello".delete("e-h").should == "llo"
    "hel-lo".delete("e-").should == "hllo"
    "hel-lo".delete("-h").should == "ello"
    "hel-lo".delete("---").should == "hello"
    "hel-012".delete("--2").should == "hel"
    "hel-()".delete("(--").should == "hel"
    "hello".delete("h-e").should == "hello"
    "hello".delete("^h-e").should == ""
    "hello".delete("^e-h").should == "he"
    "hello^".delete("^^-^").should == "^"
    "hel--lo".delete("^---").should == "--"

    "abcdefgh".delete("a-ce-fh").should == "dg"
    "abcdefgh".delete("he-fa-c").should == "dg"
    "abcdefgh".delete("e-fha-c").should == "dg"
    
    "abcde".delete("ac-e").should == "b"
    "abcde".delete("^ac-e").should == "acde"
    
    "ABCabc[]".delete("A-a").should == "bc"
  end
  
  it "taints result when self is tainted" do
    "hello".taint.delete("e").tainted?.should == true
    "hello".taint.delete("a-z").tainted?.should == true

    "hello".delete("e".taint).tainted?.should == false
  end

  it "tries to convert each set arg to a string using to_str" do
    other_string = mock('lo')
    def other_string.to_str() "lo" end
    
    other_string2 = mock('o')
    def other_string2.to_str() "o" end
    
    "hello world".delete(other_string, other_string2).should == "hell wrld"

    obj = mock('x')
    obj.should_receive(:respond_to?).with(:to_str).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_str).and_return("o")
    "hello world".delete(obj).should == "hell wrld"
  end
  
  it "raises a TypeError when one set arg can't be converted to a string" do
    lambda { "hello world".delete(?o)        }.should raise_error(TypeError)
    lambda { "hello world".delete(:o)        }.should raise_error(TypeError)
    lambda { "hello world".delete(mock('x')) }.should raise_error(TypeError)
  end
  
  it "returns subclass instances when called on a subclass" do
    StringSpecs::MyString.new("oh no!!!").delete("!").class.should == StringSpecs::MyString
  end
end

describe "String#delete!" do
  it "modifies self in place and returns self" do
    a = "hello"
    a.delete!("aeiou", "^e").should equal(a)
    a.should == "hell"
  end
  
  it "returns nil if no modifications were made" do
    a = "hello"
    a.delete!("z").should == nil
    a.should == "hello"
  end

  compliant_on :ruby, :jruby do
    it "raises a TypeError when self is frozen" do
      a = "hello"
      a.freeze

      lambda { a.delete!("")            }.should raise_error(TypeError)
      lambda { a.delete!("aeiou", "^e") }.should raise_error(TypeError)
    end
  end
end
