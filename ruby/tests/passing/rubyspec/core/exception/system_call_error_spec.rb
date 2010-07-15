require File.dirname(__FILE__) + '/../../spec_helper'

describe "SystemCallError.new" do  
  it "requires at least one argumentt" do
    lambda { SystemCallError.new }.should raise_error(ArgumentError)
  end

  it "takes an optional errno argument" do
    SystemCallError.should be_ancestor_of(SystemCallError.new("message",1).class)
  end

  it "accepts single Fixnum argument as errno" do
    SystemCallError.new(-2**24).errno.should == -2**24
    SystemCallError.new(42).errno.should == 42
    SystemCallError.new(2**24).errno.should == 2**24
  end
end

describe "SystemCallError#errno" do
  it "returns nil when no errno given" do
    SystemCallError.new("message").errno.should == nil
  end  
  
  it "returns the errno given as optional argument to new" do
    SystemCallError.new("message", -2**30).errno.should == -2**30
    SystemCallError.new("message", -1).errno.should == -1
    SystemCallError.new("message", 0).errno.should == 0
    SystemCallError.new("message", 1).errno.should == 1
    SystemCallError.new("message", 42).errno.should == 42
    SystemCallError.new("message", 2**30).errno.should == 2**30
  end
end

describe "SystemCallError#message" do
  it "should return default message when no message given" do
    SystemCallError.new(2**28).message.should =~ /Unknown error/
  end

  it "returns the message given as an argument to new" do
    SystemCallError.new("message", 1).message.should  =~ /message/
    SystemCallError.new("XXX").message.should =~ /XXX/
  end
end


