require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Singleton._instantiate?" do

  it "is private" do
    lambda { SingletonSpecs::MyClass._instantiate? }.should raise_error(NoMethodError)
  end

  # JRuby doesn't support "_instantiate?" intentionally (JRUBY-2239)
  not_compliant_on :jruby do
    it "returns nil until it is instantiated" do
      SingletonSpecs::NotInstantiated.send(:_instantiate?).should == nil
      SingletonSpecs::NotInstantiated.instance
      SingletonSpecs::NotInstantiated.send(:_instantiate?).should eql(SingletonSpecs::NotInstantiated.instance)
    end
  end
end
