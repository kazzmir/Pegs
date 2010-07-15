require File.dirname(__FILE__) + '/../../spec_helper'

describe "Proc#binding" do
  it "returns a Binding instance" do
    [Proc.new{}, lambda {}, proc {}].each { |p|
      p.binding.class.should == Binding
    }
  end

  it "returns the binding associated wiht self" do
    obj = mock('binding')
    def obj.test_binding(some, params)
      lambda {}
    end

    lambdas_binding = obj.test_binding(1, 2).binding

    eval("some", lambdas_binding).should == 1
    eval("params", lambdas_binding).should == 2
  end
end
