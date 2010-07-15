require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Array#push" do
  it "appends the arguments to the array" do
    a = [ "a", "b", "c" ]
    a.push("d", "e", "f").should equal(a)
    a.push().should == ["a", "b", "c", "d", "e", "f"]
    a.push(5)
    a.should == ["a", "b", "c", "d", "e", "f", 5]
  end

  it "isn't confused by previous shift" do
    a = [ "a", "b", "c" ]
    a.shift
    a.push("foo")
    a.should == ["b", "c", "foo"]
  end

  it "properly handles recursive arrays" do
    empty = ArraySpecs.empty_recursive_array
    empty.push(:last).should == [empty, :last]

    array = ArraySpecs.recursive_array
    array.push(:last).should == [1, 'two', 3.0, array, array, array, array, array, :last]
  end

  compliant_on :ruby, :jruby do
    it "raises a TypeError on a frozen array if modification takes place" do
      lambda { ArraySpecs.frozen_array.push(1) }.should raise_error(TypeError)
    end

    it "does not raise on a frozen array if no modification is made" do
      ArraySpecs.frozen_array.push.should == [1, 2, 3]
    end
  end
end
