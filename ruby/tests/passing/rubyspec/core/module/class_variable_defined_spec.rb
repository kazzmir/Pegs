require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Module#class_variable_defined?" do
  it "returns true if a class variable with the given name is defined in self" do
    c = Class.new { class_variable_set :@@class_var, "test" }
    c.class_variable_defined?(:@@class_var).should == true
    c.class_variable_defined?("@@class_var").should == true
    c.class_variable_defined?(:@@no_class_var).should == false
    c.class_variable_defined?("@@no_class_var").should == false
    ModuleSpecs::CVars.class_variable_defined?("@@cls").should == true
  end

  it "returns true if a class variable with the given name is defined in the metaclass" do
    ModuleSpecs::CVars.class_variable_defined?("@@meta").should == true
  end

  it "returns true if a class variables with the given name is defined in an included module" do
    c = Class.new { include ModuleSpecs::MVars }
    c.class_variable_defined?("@@mvar").should == true
  end

  it "returns false if a class variables with the given name is defined in an extended module" do
    c = Class.new
    c.extend ModuleSpecs::MVars
    c.class_variable_defined?("@@mvar").should == false
  end

  not_compliant_on :rubinius do
    it "accepts Fixnums for class variables" do
      c = Class.new { class_variable_set :@@class_var, "test" }
      c.class_variable_defined?(:@@class_var.to_i).should == true
      c.class_variable_defined?(:@@no_class_var.to_i).should == false
    end
  end

  it "raises a NameError when the given name is not allowed" do
    c = Class.new

    lambda {
      c.class_variable_defined?(:invalid_name)
    }.should raise_error(NameError)

    lambda {
      c.class_variable_defined?("@invalid_name")
    }.should raise_error(NameError)
  end

  it "converts a non string/symbol/fixnum name to string using to_str" do
    c = Class.new { class_variable_set :@@class_var, "test" }
    (o = mock('@@class_var')).should_receive(:to_str).and_return("@@class_var")
    c.class_variable_defined?(o).should == true
  end

  it "raises a TypeError when the given names can't be converted to strings using to_str" do
    c = Class.new { class_variable_set :@@class_var, "test" }
    o = mock('123')
    lambda {
      c.class_variable_defined?(o)
    }.should raise_error(TypeError)

    o.should_receive(:to_str).and_return(123)
    lambda {
      c.class_variable_defined?(o)
    }.should raise_error(TypeError)
  end
end
