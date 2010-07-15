require File.dirname(__FILE__) + '/../../spec_helper'

describe "MatchData#end" do
  it "returns the offset of the end of the nth element" do
    /(.)(.)(\d+)(\d)/.match("THX1138.").end(0).should == 7
    /(.)(.)(\d+)(\d)/.match("THX1138.").end(2).should == 3 
  end
end
