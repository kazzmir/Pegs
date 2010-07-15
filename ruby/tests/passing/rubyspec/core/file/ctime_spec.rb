require File.dirname(__FILE__) + '/../../spec_helper'

describe "File.ctime" do
  before :each do
    @file = __FILE__
  end

  after :each do
    @file = nil
  end

  it "Returns the change time for the named file (the time at which directory information about the file was changed, not the file itself)." do
    File.ctime(@file)
    File.ctime(@file).class.should == Time
  end

  it "raises an Errno::ENOENT exception if the file is not found" do
    lambda { File.ctime('bogus') }.should raise_error(Errno::ENOENT)
  end
end

describe "File#ctime" do
  before :each do
    @file = File.open(__FILE__)
  end

  after:each do
    @file.close
    @file = nil
  end

  it "Returns the change time for the named file (the time at which directory information about the file was changed, not the file itself)." do
    @file.ctime
    @file.ctime.class.should == Time
  end
end
