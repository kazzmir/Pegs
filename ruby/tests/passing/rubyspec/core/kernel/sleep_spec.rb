require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Kernel#sleep" do
  it "is a private method" do
    Kernel.private_instance_methods.should include("sleep")
  end
  
  it "pauses execution for approximately the duration requested" do
    duration = 0.01
    start = Time.now
    sleep duration
    (Time.now - start).should be_close(duration, duration)
  end
  
  it "returns the rounded number of seconds asleep" do
    sleep(0.01).should be_kind_of(Integer)
  end
  
  it "raises a TypeError when passed a non-numeric duration" do
    lambda { sleep(nil)   }.should raise_error(TypeError)
    lambda { sleep('now') }.should raise_error(TypeError)
    lambda { sleep('2')   }.should raise_error(TypeError)
  end
  
  it "pauses execution indefinitely if not given a duration" do
    lock = Channel.new
    t = Thread.new do
      lock << :ready
      sleep
      5
    end    
    lock.receive.should == :ready
    Thread.pass unless t.status == "sleep"
    t.run
    t.value.should == 5
  end
end

describe "Kernel.sleep" do
  it "needs to be reviewed for spec completeness"
end
