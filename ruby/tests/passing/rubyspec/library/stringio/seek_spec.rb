require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "StringIO#seek" do
  before(:each) do
    @io = StringIO.new("12345678")
  end
  
  it "seeks from the current position when whence is IO::SEEK_CUR" do
    @io.pos = 1
    @io.seek(1, IO::SEEK_CUR)
    @io.pos.should eql(2)
    
    @io.seek(-1, IO::SEEK_CUR)
    @io.pos.should eql(1)
  end

  it "seeks from the end of self when whence is IO::SEEK_END" do
    @io.seek(3, IO::SEEK_END)
    @io.pos.should eql(11) # Outside of the StringIO's content
    
    @io.seek(-2, IO::SEEK_END)
    @io.pos.should eql(6)
  end

  it "seeks to an absolute position when whence is IO::SEEK_SET" do
    @io.seek(5, IO::SEEK_SET)
    @io.pos.should == 5
    
    @io.pos = 3
    @io.seek(5, IO::SEEK_SET)
    @io.pos.should == 5
  end
  
  it "raises an Errno::EINVAL error on negative amounts when whence is IO::SEEK_SET" do
    lambda { @io.seek(-5, IO::SEEK_SET) }.should raise_error(Errno::EINVAL)
  end

  it "tries to convert the passed Object to a String using #to_int" do
    obj = mock("to_int")
    obj.should_receive(:to_int).and_return(2)
    @io.seek(obj)
    @io.pos.should eql(2)
  end
  
  it "raises a TypeError when the passed Object can't be converted to an Integer" do
    lambda { @io.seek(Object.new) }.should raise_error(TypeError)
  end
  
  it "checks whether the passed Object responds to #to_int" do
    obj = mock('method_missing to_int')
    obj.should_receive(:respond_to?).with(:to_int).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_int).and_return(2)
    @io.seek(obj)
    @io.pos.should eql(2)
  end
end

describe "StringIO#seek when self is closed" do
  before(:each) do
    @io = StringIO.new("example")
    @io.close
  end
  
  ruby_version_is "" ... "1.8.7" do
    it "does not raise an IOError" do
      @io.seek(5)
      @io.pos.should eql(5)
    end
  end
  
  ruby_version_is "1.8.7" do
    it "raises an IOError" do
      lambda { @io.seek(5) }.should raise_error(IOError)
    end
  end
end
