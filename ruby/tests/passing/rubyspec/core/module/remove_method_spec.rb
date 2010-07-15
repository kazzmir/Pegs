require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

module ModuleSpecs
  class NoInheritance
    def method_to_remove; 1; end

    remove_method :method_to_remove
  end
  
  class Parent
    def method_to_remove; 1; end
  end
  
  class Child < Parent
    def method_to_remove; 2; end     

    remove_method :method_to_remove
  end
  
  class First
    def method_to_remove; 1; end
  end
  
  class Second < First
    def method_to_remove; 2; end
  end
end

describe "Module#remove_method" do
  it "removes the method from a class" do
    x = ModuleSpecs::NoInheritance.new
    x.respond_to?(:method_to_remove).should == false
  end
  
  it "removes method from subclass, but not parent" do
    x = ModuleSpecs::Child.new
    x.respond_to?(:method_to_remove).should == true    
    x.method_to_remove.should == 1
  end
  
  it "raises a NameError when attempting to remove method further up the inheritance tree" do
    lambda {
      class Third < ModuleSpecs::Second
        remove_method :method_to_remove
      end
    }.should raise_error(NameError)
  end

  it "raises a NameError when attempting to remove a missing method" do
    lambda {
      class Third < ModuleSpecs::Second
        remove_method :blah
      end
    }.should raise_error(NameError)
  end
end
