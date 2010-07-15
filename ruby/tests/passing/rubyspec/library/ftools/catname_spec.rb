require File.dirname(__FILE__) + '/../../spec_helper'
require 'ftools'

describe "File.catname" do
  it "returns the 2nd arg if it's not a directory" do
    File.catname("blah", "/etc/passwd").should == "/etc/passwd"
  end
  
  it "uses File.join with the args" do
    File.catname("passwd", ".").should == "./passwd"
  end
  
  it "uses File.basename on the 1st arg before joining" do
    File.catname("etc/passwd", ".").should == "./passwd"
  end  
end
