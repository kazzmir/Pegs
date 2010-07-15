require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Module#attr_writer" do
  it "creates a setter for each given attribute name" do
    c = Class.new do
      attr_writer :test1, "test2"
    end
    o = c.new

    o.respond_to?("test1").should == false
    o.respond_to?("test2").should == false

    o.respond_to?("test1=").should == true
    o.test1 = "test_1"
    o.instance_variable_get(:@test1).should == "test_1"

    o.respond_to?("test2=").should == true
    o.test2 = "test_2"
    o.instance_variable_get(:@test2).should == "test_2"
  end

  not_compliant_on :rubinius do
    it "creates a setter for an attribute name given as a Fixnum" do
      c = Class.new do
        attr_writer :test1.to_i
      end

      o = c.new
      o.respond_to?("test1").should == false
      o.respond_to?("test1=").should == true

      o.test1 = "test_1"
      o.instance_variable_get(:@test1).should == "test_1"
    end
  end

  it "converts non string/symbol/fixnum names to strings using to_str" do
    (o = mock('test')).should_receive(:to_str).any_number_of_times.and_return("test")
    c = Class.new do
      attr_writer o
    end

    c.new.respond_to?("test").should == false
    c.new.respond_to?("test=").should == true
  end

  it "raises a TypeError when the given names can't be converted to strings using to_str" do
    o = mock('test1')
    lambda { Class.new { attr_writer o } }.should raise_error(TypeError)
    (o = mock('123')).should_receive(:to_str).and_return(123)
    lambda { Class.new { attr_writer o } }.should raise_error(TypeError)
  end
end
