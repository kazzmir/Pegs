require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/methods'

describe "Time#_load" do
  it "loads a time object in the new format" do
    t = Time.local(2000, 1, 15, 20, 1, 1)
    t = t.gmtime
    
    high =               1 << 31 |
           (t.year - 1900) << 14 |
              (t.mon  - 1) << 10 |
                    t.mday << 5 |
                    t.hour

    low =  t.min  << 26 |
           t.sec  << 20 |
           t.usec


    Time._load([high, low].pack("LL")).should == t
  end

  it "loads a time object in the old UNIX timestamp based format" do
    t = Time.local(2000, 1, 15, 20, 1, 1, 203)
    timestamp = t.to_i
    
    high = timestamp & ((1 << 31) - 1)

    low =  t.usec

    Time._load([high, low].pack("LL")).should == t
  end
end