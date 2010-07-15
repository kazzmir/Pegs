require File.dirname(__FILE__) + '/../../spec_helper'

describe "Class#allocate" do
  it "returns an instance of self" do
    klass = Class.new
    klass.allocate.should be_kind_of(klass)
  end
  
  it "returns a fully-formed instance of Module" do
    klass = Class.allocate
    klass.constants.should_not == nil
    klass.methods.should_not == nil
  end

  it "does not call initialize on the new instance" do
    klass = Class.new do
      def initialize(*args)
        @initialized = true
      end
      
      def initialized?
        @initialized || false
      end
    end
    
    klass.allocate.initialized?.should == false
  end
end