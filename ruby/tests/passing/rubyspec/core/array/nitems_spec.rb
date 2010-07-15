require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Array#nitems" do
  it "returns the number of non-nil elements" do
    [nil].nitems.should == 0
    [].nitems.should == 0    
    [1, 2, 3, nil].nitems.should == 3
    [1, 2, 3].nitems.should == 3
    [1, nil, 2, 3, nil, nil, 4].nitems.should == 4
    [1, nil, 2, false, 3, nil, nil, 4].nitems.should == 5
  end

  it "properly handles recursive arrays" do
    empty = ArraySpecs.empty_recursive_array
    empty.nitems.should == 1

    array = ArraySpecs.recursive_array
    array.nitems.should == 8

    [nil, empty, array].nitems.should == 2
  end
end
