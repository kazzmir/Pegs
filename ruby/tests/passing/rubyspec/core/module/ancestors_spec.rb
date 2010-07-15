require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Module#ancestors" do
  it "returns a list of modules included in self (including self)" do
    ModuleSpecs.ancestors.should         include(ModuleSpecs)
    ModuleSpecs::Basic.ancestors.should  include(ModuleSpecs::Basic)
    ModuleSpecs::Super.ancestors.should  include(ModuleSpecs::Super, ModuleSpecs::Basic)
    ModuleSpecs::Parent.ancestors.should include(ModuleSpecs::Parent, Object, Kernel)
    ModuleSpecs::Child.ancestors.should  include(ModuleSpecs::Child, ModuleSpecs::Super, ModuleSpecs::Basic, ModuleSpecs::Parent, Object, Kernel)
  end
  
  it "returns only modules and classes" do
    class << ModuleSpecs::Child; self; end.ancestors.should include(ModuleSpecs::Internal, Class, Module, Object, Kernel)
  end
end
