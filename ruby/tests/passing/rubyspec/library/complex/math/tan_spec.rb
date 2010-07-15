require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/shared/tan'

describe "Math#tan" do
  it_behaves_like :complex_math_tan, :_, IncludesMath.new

  it "should be private" do
    IncludesMath.private_instance_methods.should include("tan")
  end
end

describe "Math#tan!" do
  it_behaves_like :complex_math_tan_bang, :_, IncludesMath.new

  it "should be private" do
    IncludesMath.private_instance_methods.should include("tan!")
  end
end

describe "Math.tan" do
  it_behaves_like :complex_math_tan, :_, Math
end

describe "Math.tan!" do
  it_behaves_like :complex_math_tan_bang, :_, Math
end
