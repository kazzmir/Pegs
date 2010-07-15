require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Thread#key?" do
  before :each do
    @th = Thread.new do
      Thread.current[:oliver] = "a"
    end
    @th.join
  end

  it "tests for existance of thread local variables using symbols or strings" do
    @th.key?(:oliver).should == true
    @th.key?("oliver").should == true
    @th.key?(:stanley).should == false
    @th.key?(:stanley.to_s).should == false
  end

  it "raises exceptions on the wrong type of keys" do
    lambda { Thread.current.key? nil }.should raise_error(TypeError)
    lambda { Thread.current.key? 5 }.should raise_error(ArgumentError)
  end
end
