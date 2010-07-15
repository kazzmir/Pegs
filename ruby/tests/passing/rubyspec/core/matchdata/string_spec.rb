require File.dirname(__FILE__) + '/../../spec_helper'

describe "MatchData#string" do
  it "returns a copy of the match string" do
    str = /(.)(.)(\d+)(\d)/.match("THX1138.").string
    str.should == "THX1138."
  end

  compliant_on :ruby, :jruby do
    it "returns a frozen copy of the match string" do
      str = /(.)(.)(\d+)(\d)/.match("THX1138.").string
      str.should == "THX1138."
      str.frozen?.should == true
    end
  end
end
