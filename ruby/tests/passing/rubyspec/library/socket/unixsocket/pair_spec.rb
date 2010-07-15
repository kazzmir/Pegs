require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/../fixtures/classes'

describe "UNIXSocket#pair" do
  not_supported_on :windows do

    before :each do
      @s1, @s2 = UNIXSocket.pair
    end

    after :each do
      @s1.close
      @s2.close
    end

    it "returns a pair of connected sockets" do
      @s1.puts "foo"
      @s2.gets.should == "foo\n"
    end

    it "returns sockets with no name" do
      @s1.path.should == @s2.path
      @s1.path.should == ""
    end

    it "returns sockets with no address" do
      @s1.addr.should == ["AF_UNIX", ""]
      @s2.addr.should == ["AF_UNIX", ""]
    end

    it "returns sockets with no peeraddr" do
      @s1.peeraddr.should == ["AF_UNIX", ""]
      @s2.peeraddr.should == ["AF_UNIX", ""]
    end
  end
end
