require File.dirname(__FILE__) + '/../../../spec_helper'
require 'net/ftp'
require File.dirname(__FILE__) + "/fixtures/server"

# TODO: Add specs for using the SOCKSSocket
describe "Net::FTP#connect" do
  before(:each) do
    @server = NetFTPSpecs::DummyFTP.new
    @server.serve_once

    @ftp = Net::FTP.new
  end

  after(:each) do
    @server.connect_message = nil
    @ftp.quit rescue nil
    @ftp.close
    @server.stop
  end
  
  it "tries to connect to the FTP Server on the given host and port" do
    lambda { @ftp.connect("localhost", 9921) }.should_not raise_error
  end
  
  it "returns nil" do
    @ftp.connect("localhost", 9921).should be_nil
  end
  
  it "prints a small debug line when in debug mode" do
    @ftp.debug_mode = true
    lambda { @ftp.connect("localhost", 9921) }.should output(/#{"connect: localhost, 9921\\nget: 220 Dummy FTP Server ready!"}/)
    @ftp.debug_mode = false
  end

  it "does not raise any error when the response code is 220" do
    @server.connect_message = "220 Dummy FTP Server ready!"
    lambda { @ftp.connect("localhost", 9921) }.should_not raise_error
  end

  it "raises a Net::FTPReplyError when the response code is 120" do
    @server.connect_message = "120 Service ready in nnn minutes."
    lambda { @ftp.connect("localhost", 9921) }.should raise_error(Net::FTPReplyError)
  end

  it "raises a Net::FTPTempError when the response code is 421" do
    @server.connect_message = "421 Service not available, closing control connection."
    lambda { @ftp.connect("localhost", 9921) }.should raise_error(Net::FTPTempError)
  end
end