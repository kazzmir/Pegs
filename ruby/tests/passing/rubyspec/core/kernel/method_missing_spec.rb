require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Kernel#method_missing" do
  it "is a private method" do
    Kernel.private_instance_methods.should include("method_missing")
  end
  
  it "is not called when a defined method is called" do
    KernelSpecs::MethodMissing.should_not_receive(:method_missing)
    KernelSpecs::MethodMissingC.should_not_receive(:method_missing)
    obj = KernelSpecs::MethodMissingC.new
    obj.should_not_receive(:method_missing)
    
    KernelSpecs::MethodMissing.existing.should == :existing
    KernelSpecs::MethodMissingC.existing.should == :existing
    obj.existing.should == :instance_existing
  end
  
  it "is called when an undefined method is called" do
    KernelSpecs::MethodMissing.nonexistent.should == :method_missing
    KernelSpecs::MethodMissingC.nonexistent.should == :method_missing
    KernelSpecs::MethodMissingC.new.nonexistent.should == :instance_method_missing
  end
  
  it "is called when a private method is called" do
    KernelSpecs::MethodMissing.private_method.should == :method_missing
    KernelSpecs::MethodMissingC.private_method.should == :method_missing
    KernelSpecs::MethodMissingC.new.private_method.should == :instance_method_missing
  end
  
  it "is called when a protected method is called" do
    KernelSpecs::MethodMissingC.protected_method.should == :method_missing
    KernelSpecs::MethodMissingC.new.protected_method.should == :instance_method_missing
  end
end

describe "When Kernel#method_missing is undefined" do
  it "a NoMethodError is raised when an undefined method is called" do
    lambda { KernelSpecs::NoMethodMissing.nonexistent      }.should raise_error(NoMethodError)
    lambda { KernelSpecs::NoMethodMissingC.nonexistent     }.should raise_error(NoMethodError)
    lambda { KernelSpecs::NoMethodMissingC.new.nonexistent }.should raise_error(NoMethodError)
  end
  
  it "a NoMethodError is raised when a private method is called" do
    lambda { KernelSpecs::NoMethodMissing.nonexistent      }.should raise_error(NoMethodError)
    lambda { KernelSpecs::NoMethodMissingC.nonexistent     }.should raise_error(NoMethodError)
    lambda { KernelSpecs::NoMethodMissingC.new.nonexistent }.should raise_error(NoMethodError)
  end
  
  it "a NoMethodError is raised when a protected method is called" do
    lambda { KernelSpecs::NoMethodMissing.nonexistent      }.should raise_error(NoMethodError)
    lambda { KernelSpecs::NoMethodMissingC.nonexistent     }.should raise_error(NoMethodError)
    lambda { KernelSpecs::NoMethodMissingC.new.nonexistent }.should raise_error(NoMethodError)
  end
end

describe "Kernel.method_missing" do
  it "needs to be reviewed for spec completeness"
end
