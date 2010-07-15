require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#strip" do
  it "returns a new string with leading and trailing whitespace removed" do
    "   hello   ".strip.should == "hello"
    "   hello world   ".strip.should == "hello world"
    "\tgoodbye\r\v\n".strip.should == "goodbye"
    "  goodbye \000".strip.should == "goodbye"
  end
  
  it "taints the result when self is tainted" do
    "".taint.strip.tainted?.should == true
    "ok".taint.strip.tainted?.should == true
    "  ok  ".taint.strip.tainted?.should == true
  end
end

describe "String#strip!" do
  it "modifies self in place and returns self" do
    a = "   hello   "
    a.strip!.should equal(a)
    a.should == "hello"
  end
  
  it "returns nil if no modifications where made" do
    a = "hello"
    a.strip!.should == nil
    a.should == "hello"
  end

  compliant_on :ruby, :jruby do
    it "raises a TypeError if self is frozen" do
      "hello".freeze.strip! # ok, nothing changed
      "".freeze.strip! # ok, nothing changed

      lambda { "  hello  ".freeze.strip! }.should raise_error(TypeError)
    end
  end
end
