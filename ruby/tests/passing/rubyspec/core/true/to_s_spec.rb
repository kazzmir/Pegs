require File.dirname(__FILE__) + '/../../spec_helper'

describe "TrueClass#to_s" do
  it "returns the string 'true'" do
    true.to_s.should == "true"
  end
end
