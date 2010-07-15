require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Module#alias_method" do
  before(:each) do
    @class = Class.new(ModuleSpecs::Aliasing)
    @object = @class.new 
  end
  
  it "makes a copy of the method" do
    @class.make_alias :uno, :public_one
    @class.make_alias :double, :public_two
    @object.uno.should == @object.public_one
    @object.double(12).should == @object.public_two(12)
  end

  it "retains method visibility" do
    @class.make_alias :private_ichi, :private_one
    lambda { @object.private_one  }.should raise_error(NameError)
    lambda { @object.private_ichi }.should raise_error(NameError)
    @class.make_alias :public_ichi, :public_one
    @object.public_ichi.should == @object.public_one
    @class.make_alias :protected_ichi, :protected_one
    lambda { @object.protected_ichi }.should raise_error(NameError)
  end
  
  it "fails if origin method not found" do
    lambda { @class.make_alias :ni, :san }.should raise_error(NameError)
  end

  it "converts a non string/symbol/fixnum name to string using to_str" do
    @class.make_alias "un", "public_one"
    @class.make_alias :deux, "public_one"
    @class.make_alias "trois", :public_one
    @class.make_alias :quatre, :public_one
    name = mock('cinq')
    name.should_receive(:to_str).any_number_of_times.and_return("cinq")
    @class.make_alias name, "public_one"
    @class.make_alias "cinq", name
  end

  it "raises a TypeError when the given name can't be converted using to_str" do
    lambda { @class.make_alias mock('x'), :public_one }.should raise_error(TypeError)
  end

  it "is a private method" do
    lambda { @class.alias_method :ichi, :public_one }.should raise_error(NoMethodError)
  end

  it "works in module" do
    ModuleSpecs::Allonym.new.publish.should == :report
  end
  
  it "works on private module methods in a module that has been reopened" do
    ModuleSpecs::ReopeningModule.foo.should == true
    lambda { ModuleSpecs::ReopeningModule.foo2 }.should_not raise_error(NoMethodError)
  end
  
end
