require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Module#included_modules" do
  it "returns a list of modules included in self" do
    ModuleSpecs.included_modules.should         == []
    ModuleSpecs::Child.included_modules.should  include(ModuleSpecs::Super, ModuleSpecs::Basic, Kernel)
    ModuleSpecs::Parent.included_modules.should include(Kernel)
    ModuleSpecs::Basic.included_modules.should  == []
    ModuleSpecs::Super.included_modules.should  include(ModuleSpecs::Basic)
  end
end
