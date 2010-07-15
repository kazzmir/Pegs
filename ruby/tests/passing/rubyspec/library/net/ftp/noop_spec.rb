require File.dirname(__FILE__) + '/../../../spec_helper'
require 'net/ftp'
require File.dirname(__FILE__) + "/fixtures/server"

describe "Net::FTP#noop" do
  before(:each) do
    @server = NetFTPSpecs::DummyFTP.new
    @server.serve_once

    @ftp = Net::FTP.new
    @ftp.connect("localhost", 9921)
  end

  after(:each) do
    @ftp.quit rescue nil
    @ftp.close
    @server.stop
  end

  it "sends the NOOP command to the server" do
    @ftp.noop
    @ftp.last_response.should == "200 Command okay. (NOOP)\n"
  end
  
  it "returns nil" do
    @ftp.noop.should be_nil
  end
  
  it "raises a Net::FTPPermError when the response code is 500" do
    @server.should_receive(:noop).and_respond("500 Syntax error, command unrecognized.")
    lambda { @ftp.noop }.should raise_error(Net::FTPPermError)
  end

  it "raises a Net::FTPTempError when the response code is 421" do
    @server.should_receive(:noop).and_respond("421 Service not available, closing control connection.")
    lambda { @ftp.noop }.should raise_error(Net::FTPTempError)
  end
end
