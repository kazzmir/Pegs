require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/methods'

describe "Time#to_a" do
  it "returns a 10 element array representing the deconstructed time" do
    # Testing with America/Regina here because it doesn't have DST.
    with_timezone("America/Regina") do
      Time.at(0).to_a.should == [0, 0, 18, 31, 12, 1969, 3, 365, false, "CST"]
    end
  end
end
