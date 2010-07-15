require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "UnboundMethod#bind" do
  before :each do
    @normal_um = UnboundMethodSpecs::Methods.new.method(:foo).unbind
    @parent_um = UnboundMethodSpecs::Parent.new.method(:foo).unbind
    @child1_um = UnboundMethodSpecs::Child1.new.method(:foo).unbind
    @child2_um = UnboundMethodSpecs::Child2.new.method(:foo).unbind
  end

  it "raises TypeError if object is not kind_of? the Module the method defined in" do
    lambda { @normal_um.bind(UnboundMethodSpecs::B.new) }.should raise_error(TypeError)
  end

  it "returns Method for any object that is kind_of? the Module method was extracted from" do
    @normal_um.bind(UnboundMethodSpecs::Methods.new).class.should == Method
  end

  deviates_on :rubinius do
    it "returns Method for any object kind_of? the Module the method is defined in" do
      @parent_um.bind(UnboundMethodSpecs::Child1.new).class.should == Method
      @child1_um.bind(UnboundMethodSpecs::Parent.new).class.should == Method
      @child2_um.bind(UnboundMethodSpecs::Child1.new).class.should == Method
    end
  end

  it "Method returned for obj is equal to one directly returned by obj.method" do
    obj = UnboundMethodSpecs::Methods.new
    @normal_um.bind(obj).should == obj.method(:foo)
  end
end
