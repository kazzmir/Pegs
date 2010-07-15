require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Array#compact" do
  it "returns a copy of array with all nil elements removed" do
    a = [1, 2, 4]
    a.compact.should == [1, 2, 4]
    a = [1, nil, 2, 4]
    a.compact.should == [1, 2, 4]
    a = [1, 2, 4, nil]
    a.compact.should == [1, 2, 4]
    a = [nil, 1, 2, 4]
    a.compact.should == [1, 2, 4]
  end

  it "returns subclass instance for Array subclasses" do
    ArraySpecs::MyArray[1, 2, 3, nil].compact.class.should == ArraySpecs::MyArray
  end
end

describe "Array#compact!" do
  it "removes all nil elements" do
    a = ['a', nil, 'b', false, 'c']
    a.compact!.should equal(a)
    a.should == ["a", "b", false, "c"]
    a = [nil, 'a', 'b', false, 'c']
    a.compact!.should equal(a)
    a.should == ["a", "b", false, "c"]
    a = ['a', 'b', false, 'c', nil]
    a.compact!.should equal(a)
    a.should == ["a", "b", false, "c"]
  end
  
  it "returns nil if there are no nil elements to remove" do
    [1, 2, false, 3].compact!.should == nil
  end

  compliant_on :ruby, :jruby do
    it "raises a TypeError on a frozen array" do
      lambda { ArraySpecs.frozen_array.compact! }.should raise_error(TypeError)
    end
  end
end
