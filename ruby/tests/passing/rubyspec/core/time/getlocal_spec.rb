require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/methods'

describe "Time#getlocal" do
  it "returns a new time which is the local representation of time" do
    # Testing with America/Regina here because it doesn't have DST.
    with_timezone("CST", -6) do
      t = Time.gm(2007, 1, 9, 12, 0, 0)
      t.localtime.should == Time.local(2007, 1, 9, 6, 0, 0)
    end
  end
end
