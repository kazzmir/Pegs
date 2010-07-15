require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "StringIO#close" do
  before(:each) do
    @io = StringIOSpecs.build
  end
  
  it "returns nil" do
    @io.close.should be_nil
  end
  
  it "prevents further reading and/or writing" do
    @io.close
    lambda { @io.read(1) }.should raise_error(IOError)
    lambda { @io.write('x') }.should raise_error(IOError)
  end

  it "raises an IOError when self was already closed" do
    @io.close
    lambda { @io.close }.should raise_error(IOError)
  end
end
