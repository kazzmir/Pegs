require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Kernel#singleton_methods" do
  it "returns a list of the names of singleton methods in the object" do
    m = KernelSpecs::Methods.singleton_methods(false)
    ["hachi", "ichi", "juu", "juu_ichi", "juu_ni", "roku", "san", "shi"].each do |e|
      m.should include(e)
    end
    
    KernelSpecs::Methods.new.singleton_methods(false).should == []
  end
  
  it "should handle singleton_methods call with and without argument" do
    [1, "string", :symbol, [], {} ].each do |e|
      lambda {e.singleton_methods}.should_not raise_error
      lambda {e.singleton_methods(true)}.should_not raise_error
      lambda {e.singleton_methods(false)}.should_not raise_error
    end
    
  end
  
  it "returns a list of the names of singleton methods in the object and its ancestors and mixed-in modules" do
    m = (KernelSpecs::Methods.singleton_methods(false) & KernelSpecs::Methods.singleton_methods)
    ["hachi", "ichi", "juu", "juu_ichi", "juu_ni", "roku", "san", "shi"].each do |e|
      m.should include(e)
    end
    
    KernelSpecs::Methods.new.singleton_methods.should == []
  end
end
