require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/../fixtures/classes'

describe "UNIXSocket#recvfrom" do

  not_supported_on :windows do
    before :each do
      @path = SocketSpecs.socket_path

      @server = UNIXServer.open(@path)
      @client = UNIXSocket.open(@path)
      @socket = @server.accept_nonblock  
      @client.send("foobar", 0)
    end
  
    after :each do
      @socket.close
      @client.close
      @server.close
      File.unlink(@path) if File.exists?(@path)
    end
    
    it "accepts a connection in a non-blocking way" do
      data = @socket.recvfrom(6).first
      data.should == "foobar"
    end
  
    it "returns a UNIXSocket" do
      @socket.should be_kind_of(UNIXSocket)    
    end
  end
end
