require File.dirname(__FILE__) + '/../../spec_helper'

describe "MatchData#pre_match" do
  it "returns the string before the match, equiv. special var $`" do
    /(.)(.)(\d+)(\d)/.match("THX1138: The Movie").pre_match.should == 'T'
    $`.should == 'T'
  end
end
