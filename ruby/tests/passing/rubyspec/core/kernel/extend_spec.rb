require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'
module KernelSpecs::M
  def self.extend_object(o)
    ScratchPad << "extend_object"
    super
  end

  def self.extended(o)
    ScratchPad << "extended"
    super
  end
end

describe "Kernel#extend" do
  before(:each) do
    ScratchPad.record []
  end

  it "calls extend_object on argument" do
    o = mock('o')
    o.extend KernelSpecs::M
    ScratchPad.recorded.include?("extend_object").should == true
  end

  it "calls extended on argument" do
    o = mock('o')
    o.extend KernelSpecs::M
    ScratchPad.recorded.include?("extended").should == true
  end

  it "makes the class a kind_of? the argument" do
    class C
      extend KernelSpecs::M
    end
    (C.kind_of? KernelSpecs::M).should == true
  end

  compliant_on :ruby, :jruby do
    it "raises a TypeError if self is frozen" do
      module KernelSpecs::Mod; end
      o = mock('o')
      o.freeze
      lambda { o.extend KernelSpecs::Mod }.should raise_error(TypeError)
    end
  end
end

describe "Kernel#extend" do
  it "needs to be reviewed for spec completeness"
end
