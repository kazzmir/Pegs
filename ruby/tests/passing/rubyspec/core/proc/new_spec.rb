require File.dirname(__FILE__) + '/fixtures/procs.rb'
require File.dirname(__FILE__) + '/../../spec_helper'

describe "Proc.new with an associated block" do
  it "returns a proc that represents the block" do
    Proc.new { }.call.should == nil
    Proc.new { "hello" }.call.should == "hello"
  end

  it "raises a LocalJumpError when context of the block no longer exists" do
    def some_method(&b) b end
    a_proc = Proc.new { return } 
    res = some_method(&a_proc)

    lambda { res.call }.should raise_error(LocalJumpError)
  end

  it "returns from within enclosing method when 'return' is used in the block" do
    # we essentially verify that the created instance behaves like proc,
    # not like lambda.
    def some_method
      Proc.new { return :proc_return_value }.call
      :method_return_value
    end
    some_method.should == :proc_return_value
  end

end

describe "Proc.new without a block" do
  it "raises an ArgumentError" do
    lambda { Proc.new }.should raise_error(ArgumentError)
  end

  it "raises an ArgumentError if invoked from within a method with no block" do
    lambda {
      ProcSpecs.new_proc_in_method
    }.should raise_error(ArgumentError)
  end

  it "returns a new Proc instance from the block passed to the containing method" do
    ProcSpecs.new_proc_in_method { "hello" }.call.should == "hello"
  end

end
