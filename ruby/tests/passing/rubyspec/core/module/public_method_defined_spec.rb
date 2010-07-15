require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Module#public_method_defined?" do
  it "returns true if the named public method is defined by module or its ancestors" do
    ModuleSpecs::CountsMixin.public_method_defined?("public_3").should == true
    
    ModuleSpecs::CountsParent.public_method_defined?("public_3").should == true
    ModuleSpecs::CountsParent.public_method_defined?("public_2").should == true
    
    ModuleSpecs::CountsChild.public_method_defined?("public_3").should == true
    ModuleSpecs::CountsChild.public_method_defined?("public_2").should == true
    ModuleSpecs::CountsChild.public_method_defined?("public_1").should == true
  end

  it "returns false if method is not a public method" do
    ModuleSpecs::CountsChild.public_method_defined?("private_3").should == false
    ModuleSpecs::CountsChild.public_method_defined?("private_2").should == false
    ModuleSpecs::CountsChild.public_method_defined?("private_1").should == false

    ModuleSpecs::CountsChild.public_method_defined?("protected_3").should == false
    ModuleSpecs::CountsChild.public_method_defined?("protected_2").should == false
    ModuleSpecs::CountsChild.public_method_defined?("protected_1").should == false
  end

  it "returns false if the named method is not defined by the module or its ancestors" do
    ModuleSpecs::CountsMixin.public_method_defined?(:public_10).should == false
  end

  it "accepts symbols for the method name" do
    ModuleSpecs::CountsMixin.public_method_defined?(:public_3).should == true
  end

  compliant_on :ruby, :jruby do
    it "raises an ArgumentError if called with a Fixnum" do
      lambda { ModuleSpecs::CountsMixin.public_method_defined?(1)     }.should raise_error(ArgumentError)
    end
    
    it "raises a TypeError if not passed a Symbol" do
      lambda { ModuleSpecs::CountsMixin.public_method_defined?(nil)   }.should raise_error(TypeError)
      lambda { ModuleSpecs::CountsMixin.public_method_defined?(false) }.should raise_error(TypeError)

      sym = mock('symbol')
      def sym.to_sym() :public_3 end
      lambda { ModuleSpecs::CountsMixin.public_method_defined?(sym) }.should raise_error(TypeError)
    end
  end
  
  it "accepts any object that is a String type" do
    str = mock('public_3')
    def str.to_str() 'public_3' end
    ModuleSpecs::CountsMixin.public_method_defined?(str).should == true
  end
  
  deviates_on :rubinius do
    it "raises a TypeError if not passed a String type" do
      lambda { ModuleSpecs::CountsMixin.public_method_defined?(mock('x')) }.should raise_error(TypeError)
    end
  end
end
