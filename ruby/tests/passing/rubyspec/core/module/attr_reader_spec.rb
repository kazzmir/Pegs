require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Module#attr_reader" do
  it "creates a getter for each given attribute name" do
    c = Class.new do
      attr_reader :a, "b"
      
      def initialize
        @a = "test"
        @b = "test2"
      end
    end
    
    o = c.new
    %w{a b}.each do |x|
      o.respond_to?(x).should == true
      o.respond_to?("#{x}=").should == false
    end

    o.a.should == "test"
    o.b.should == "test2"
  end

  it "converts non string/symbol/fixnum names to strings using to_str" do
    (o = mock('test')).should_receive(:to_str).any_number_of_times.and_return("test")
    c = Class.new do
      attr_reader o
    end
    
    c.new.respond_to?("test").should == true
    c.new.respond_to?("test=").should == false
  end

  it "raises a TypeError when the given names can't be converted to strings using to_str" do
    o = mock('o')
    lambda { Class.new { attr_reader o } }.should raise_error(TypeError)
    (o = mock('123')).should_receive(:to_str).and_return(123)
    lambda { Class.new { attr_reader o } }.should raise_error(TypeError)
  end
end
