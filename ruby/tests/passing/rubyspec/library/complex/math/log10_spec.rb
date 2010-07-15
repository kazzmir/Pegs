require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/shared/log10'

describe "Math#log10" do
  it_behaves_like :complex_math_log10, :_, IncludesMath.new

  it "should be private" do
    IncludesMath.private_instance_methods.should include("log10")
  end
end

describe "Math#log10!" do
  it_behaves_like :complex_math_log10_bang, :_, IncludesMath.new

  it "should be private" do
    IncludesMath.private_instance_methods.should include("log10!")
  end
end

describe "Math.log10" do
  it_behaves_like :complex_math_log10, :_, Math
end

describe "Math.log10!" do
  it_behaves_like :complex_math_log10_bang, :_, Math
end
