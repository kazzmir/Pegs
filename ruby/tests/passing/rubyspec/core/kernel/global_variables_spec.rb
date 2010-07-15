require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Kernel.global_variables" do
  it "is a private method" do
    Kernel.private_instance_methods.should include("global_variables")
  end
  
  it "finds subset starting with std" do
    global_variables.grep(/std/).should include("$stderr", "$stdin", "$stdout")
    a = global_variables.size
    global_variables.include?("$foolish_global_var").should == false
    eval("$foolish_global_var = 1")
    global_variables.size.should == a+1
    global_variables.should include("$foolish_global_var")
  end
end

describe "Kernel#global_variables" do
  it "needs to be reviewed for spec completeness"
end
