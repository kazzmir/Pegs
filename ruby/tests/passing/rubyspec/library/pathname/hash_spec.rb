require File.dirname(__FILE__) + '/../../spec_helper'
require 'pathname'

describe "Pathname#hash" do

  it "should equal the hash of the pathname" do
    Pathname.new('/usr/local/bin/').hash.should == '/usr/local/bin/'.hash
  end

  it "should not equal the hash of a different pathname" do
    Pathname.new('/usr/local/bin/').hash.should_not == '/usr/bin/'.hash
  end

end

