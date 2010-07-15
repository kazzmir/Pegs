require File.dirname(__FILE__) + '/../../spec_helper'

describe "ENV.values" do

  it "returns an array of the values" do
    orig = ENV.to_hash
    begin
      ENV.replace "a" => "b", "c" => "d"
      a = ENV.values
      a.sort.should == ["b", "d"]
    ensure
      ENV.replace orig
    end
  end
end
