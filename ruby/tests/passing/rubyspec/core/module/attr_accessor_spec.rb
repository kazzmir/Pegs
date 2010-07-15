require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Module#attr_accessor" do
  it "creates a getter and setter for each given attribute name" do
    c = Class.new do
      attr_accessor :a, "b"
    end
    
    o = c.new
    
    ['a','b'].each do |x|
      o.respond_to?(x).should == true
      o.respond_to?("#{x}=").should == true
    end
    
    o.a = "a"
    o.a.should == "a"

    o.b = "b"
    o.b.should == "b"
  end
  
  it "converts non string/symbol/fixnum names to strings using to_str" do
    (o = mock('test')).should_receive(:to_str).any_number_of_times.and_return("test")
    c = Class.new do
      attr_accessor o
    end
    
    c.new.respond_to?("test").should == true
    c.new.respond_to?("test=").should == true
  end

  it "raises a TypeError when the given names can't be converted to strings using to_str" do
    o = mock('o')
    lambda { Class.new { attr_accessor o } }.should raise_error(TypeError)
    (o = mock('123')).should_receive(:to_str).and_return(123)
    lambda { Class.new { attr_accessor o } }.should raise_error(TypeError)
  end
end
