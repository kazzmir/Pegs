require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/../../shared/file/setgid'
 
describe "File.setgid?" do
  it_behaves_like :file_setgid, :setgid?, File
end
 
describe "File.setgid?" do
  before(:each) do
    @name = 'test.txt'
    @file = File.new(@name, "w")
  end
  
  after(:each) do
    File.delete(@name) if File.exists?(@name)
  end
  
  it "should return false if the file was just made" do
    File.setgid?(@name).should == false
  end
  
  it "should be false if the file doesn't exist" do
    File.delete(@name) # delete it prematurely, just for this part
    File.setgid?(@name).should == false
  end
  
  platform_is_not :windows do
    it "should return true when the gid bit is set" do
      system "chmod g+s #{@name}"
      
      File.setgid?(@name).should == true
    end
  end
end
