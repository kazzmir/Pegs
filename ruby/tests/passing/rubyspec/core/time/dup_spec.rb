require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/methods'

describe "Time#dup" do
  it "returns a Time object that represents the same time" do
	  t = Time.at(100)
	  t.dup.tv_sec.should == t.tv_sec
  end

  it "copies the gmt state flag" do
	  Time.now.gmtime.dup.gmt?.should == true
  end

  it "returns an independent Time object" do
	  t = Time.now
	  t2 = t.dup
	  t.gmtime

	  t2.gmt?.should == false
  end
end
