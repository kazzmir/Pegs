describe :array_clone, :shared => true do
  it "returns an Array or a subclass instance" do
    [].send(@method).class.should == Array
    ArraySpecs::MyArray[1, 2].send(@method).class.should == ArraySpecs::MyArray
  end

  it "produces a shallow copy where the references are directly copied" do
    a = [mock('1'), mock('2')]
    b = a.send @method
    b.first.object_id.should == a.first.object_id
    b.last.object_id.should == a.last.object_id
  end

  it "creates a new array containing all elements or the original" do
    a = [1, 2, 3, 4]
    b = a.send @method
    b.should == a
    b.__id__.should_not == a.__id__
  end

  it "copies taint status from the original" do
    a = [1, 2, 3, 4]
    b = [1, 2, 3, 4]
    a.taint
    aa = a.send @method
    bb = b.send @method

    aa.tainted?.should == true
    bb.tainted?.should == false
  end
end
