require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Kernel#eql?" do 
  it "returns true if obj and anObject are the same object." do 
    o1 = mock('o1')
    o2 = mock('o2')
    o1.should eql(o1)
    o2.should eql(o2)
    o1.should_not eql(o2)
  end
  
  it "returns true if obj and anObject have the same value." do
    :hola.should_not eql(1)
    1.should eql(1)
    :hola.should eql(:hola)
  end
end

