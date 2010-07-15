require File.dirname(__FILE__) + '/../../spec_helper'

describe "ENV.fetch" do

  it "returns a value" do
    ENV["foo"] = "bar"
    ENV.fetch("foo").should == "bar"
    ENV.delete "foo"
  end

  it "raises IndexError for an invalid key" do
    lambda { ENV.fetch "should_never_be_set" }.should raise_error(IndexError)
  end

  it "provides the given default parameter" do
    ENV.fetch("should_never_be_set", "default").should == "default"
  end

  it "provides a default value from a block" do
    ENV.fetch("should_never_be_set") { |k| "wanted #{k}" }.should == "wanted should_never_be_set"
  end

  it "warns on block and default parameter given" do
    lambda do
       ENV.fetch("should_never_be_set", "default") { 1 }.should == 1
    end.should complain(/block supersedes default value argument/)
  end

end
