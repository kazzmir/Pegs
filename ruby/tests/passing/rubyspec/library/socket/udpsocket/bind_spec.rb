require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/../fixtures/classes'

describe "UDPSocket.bind" do
  
  before :each do
    @socket = UDPSocket.new
  end

  after :each do
    @socket.close unless @socket.closed?
  end

  it "binds the socket to a port" do
    @socket.bind(SocketSpecs.hostname, SocketSpecs.port)

    lambda { @socket.bind(SocketSpecs.hostname, SocketSpecs.port) }.should raise_error
  end

  it "receives a hostname and a port" do
    @socket.bind(SocketSpecs.hostname, SocketSpecs.port)
    
    port, host = Socket.unpack_sockaddr_in(@socket.getsockname)
    
    host.should == "127.0.0.1"
    port.should == SocketSpecs.port
  end

  it "binds to INADDR_ANY if the hostname is empty" do
    @socket.bind("", SocketSpecs.port)
    port, host = Socket.unpack_sockaddr_in(@socket.getsockname)
    host.should == "0.0.0.0"    
  end
end
