require File.dirname(__FILE__) + '/../../../spec_helper'
require 'net/ftp'

describe "Net::FTP#return_code" do
  before(:each) do
    @ftp = Net::FTP.new
  end
  
  it "outputs a warning and returns \\n" do
    lambda do
      @ftp.return_code.should == "\n"
    end.should complain("warning: Net::FTP#return_code is obsolete and do nothing\n")
  end
end

describe "Net::FTP#return_code=" do
  before(:each) do
    @ftp = Net::FTP.new
  end

  it "outputs a warning" do
    lambda { @ftp.return_code = 123 }.should complain("warning: Net::FTP#return_code= is obsolete and do nothing\n")
  end
end
