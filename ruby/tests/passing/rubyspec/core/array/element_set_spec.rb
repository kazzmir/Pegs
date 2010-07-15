require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Array#[]=" do
  it "sets the value of the element at index" do
    a = [1, 2, 3, 4]
    a[2] = 5
    a[-1] = 6
    a[5] = 3
    a.should == [1, 2, 5, 6, nil, 3]

    a = []
    a[4] = "e"
    a.should == [nil, nil, nil, nil, "e"]
    a[3] = "d"
    a.should == [nil, nil, nil, "d", "e"]
    a[0] = "a"
    a.should == ["a", nil, nil, "d", "e"]
    a[-3] = "C"
    a.should == ["a", nil, "C", "d", "e"]
    a[-1] = "E"
    a.should == ["a", nil, "C", "d", "E"]
    a[-5] = "A"
    a.should == ["A", nil, "C", "d", "E"]
    a[5] = "f"
    a.should == ["A", nil, "C", "d", "E", "f"]
    a[1] = []
    a.should == ["A", [], "C", "d", "E", "f"]
    a[-1] = nil
    a.should == ["A", [], "C", "d", "E", nil]
  end
  
  it "removes the section defined by start, length when set to nil" do
    a = ['a', 'b', 'c', 'd', 'e']
    a[1, 3] = nil
    a.should == ["a", "e"]
  end
  
  it "sets the section defined by start, length to other" do
    a = [1, 2, 3, 4, 5, 6]
    a[0, 1] = 2
    a[3, 2] = ['a', 'b', 'c', 'd']
    a.should == [2, 2, 3, "a", "b", "c", "d", 6]
  end
  
  it "removes the section defined by range when set to nil" do
    a = [1, 2, 3, 4, 5]
    a[0..1] = nil
    a.should == [3, 4, 5]
  end
  
  it "sets the section defined by range to other" do
    a = [6, 5, 4, 3, 2, 1]
    a[1...2] = 9
    a[3..6] = [6, 6, 6]
    a.should == [6, 9, 4, 6, 6, 6]
  end

  it "calls to_int on its start and length arguments" do
    obj = mock('to_int')
    obj.stub!(:to_int).and_return(2)
      
    a = [1, 2, 3, 4]
    a[obj, 0] = [9]
    a.should == [1, 2, 9, 3, 4]
    a[obj, obj] = []
    a.should == [1, 2, 4]
    a[obj] = -1
    a.should == [1, 2, -1]
  end

  it "sets elements in the range arguments when passed ranges" do
    ary = [1, 2, 3]
    rhs = [nil, [], ["x"], ["x", "y"]]
    (0 .. ary.size + 2).each do |a|
      (a .. ary.size + 3).each do |b|
        rhs.each do |c|
          ary1 = ary.dup
          ary1[a .. b] = c
          ary2 = ary.dup
          ary2[a, 1 + b-a] = c
          ary1.should == ary2
          
          ary1 = ary.dup
          ary1[a ... b] = c
          ary2 = ary.dup
          ary2[a, b-a] = c
          ary1.should == ary2
        end
      end
    end

    # Now we only have to test cases where the start, length interface would
    # have raise an exception because of negative size
    ary[1...1] = [5]
    ary.should == [1, 5, 2, 3]
    ary[1..0] = [4, 3]
    ary.should == [1, 4, 3, 5, 2, 3]
    ary[-1..0] = nil
    ary.should == [1, 4, 3, 5, 2, 3]
    ary[-3..2] = []
    ary.should == [1, 4, 3, 5, 2, 3]
    ary[4..2] = []
    ary.should == [1, 4, 3, 5, 2, 3]
  end

  it "tries to convert Range elements to Integers using #to_int with [m..n] and [m...n]" do
    from = mock('from')
    to = mock('to')

    # So we can construct a range out of them...
    def from.<=>(o) 0 end
    def to.<=>(o) 0 end

    def from.to_int() 1 end
    def to.to_int() -2 end
      
    a = [1, 2, 3, 4]
      
    a[from .. to] = ["a", "b", "c"]
    a.should == [1, "a", "b", "c", 4]

    a[to .. from] = ["x"]
    a.should == [1, "a", "b", "x", "c", 4]
    lambda { a["a" .. "b"] = []  }.should raise_error(TypeError)
    lambda { a[from .. "b"] = [] }.should raise_error(TypeError)
  end

  it "checks whether the Range elements respond to #to_int with [m..n] and [m...n]" do
    from = mock('from')
    to = mock('to')

    def from.<=>(o) 0 end
    def to.<=>(o) 0 end

    from.should_receive(:respond_to?).with(:to_int).any_number_of_times.and_return(true)
    from.should_receive(:method_missing).with(:to_int).and_return(1)

    to.should_receive(:respond_to?).with(:to_int).any_number_of_times.and_return(true)
    to.should_receive(:method_missing).with(:to_int).and_return(-2)

    [1, 2, 3, 4][from .. to] = ["a", "b", "c"]
  end

  it "raises an IndexError when passed indexes out of bounds" do
    a = [1, 2, 3, 4]
    lambda { a[-5] = ""      }.should raise_error(IndexError)
    lambda { a[-5, -1] = ""  }.should raise_error(IndexError)
    lambda { a[-5, 0] = ""   }.should raise_error(IndexError)
    lambda { a[-5, 1] = ""   }.should raise_error(IndexError)
    lambda { a[-5, 2] = ""   }.should raise_error(IndexError)
    lambda { a[-5, 10] = ""  }.should raise_error(IndexError)
    
    lambda { a[-5..-5] = ""  }.should raise_error(RangeError)
    lambda { a[-5...-5] = "" }.should raise_error(RangeError)
    lambda { a[-5..-4] = ""  }.should raise_error(RangeError)
    lambda { a[-5...-4] = "" }.should raise_error(RangeError)
    lambda { a[-5..10] = ""  }.should raise_error(RangeError)
    lambda { a[-5...10] = "" }.should raise_error(RangeError)
    
    # ok
    a[0..-9] = [1]
    a.should == [1, 1, 2, 3, 4]
  end
  
  it "calls to_ary on its rhs argument for multi-element sets" do
    obj = mock('to_ary')
    def obj.to_ary() [1, 2, 3] end
    ary = [1, 2]
    ary[0, 0] = obj
    ary.should == [1, 2, 3, 1, 2]
    ary[1, 10] = obj
    ary.should == [1, 1, 2, 3]
  end
  
  it "does not call to_ary on rhs array subclasses for multi-element sets" do
    ary = []
    ary[0, 0] = ArraySpecs::ToAryArray[5, 6, 7]
    ary.should == [5, 6, 7]
  end

  compliant_on :ruby, :jruby do
    it "raises a TypeError on a frozen array" do
      lambda { ArraySpecs.frozen_array[0, 0] = [] }.should raise_error(TypeError)
    end  
  end
end

describe "Array#[]= with [index]" do
  it "returns value assigned if idx is inside array" do
    a = [1, 2, 3, 4, 5]
    (a[3] = 6).should == 6
  end
  
  it "returns value assigned if idx is right beyond right array boundary" do
    a = [1, 2, 3, 4, 5]
    (a[5] = 6).should == 6
  end
  
  it "returns value assigned if idx far beyond right array boundary" do
    a = [1, 2, 3, 4, 5]
    (a[10] = 6).should == 6
  end

  it "sets the value of the element at index" do
      a = [1, 2, 3, 4]
      a[2] = 5
      a[-1] = 6
      a[5] = 3
      a.should == [1, 2, 5, 6, nil, 3]
    end

  it "sets the value of the element if it is right beyond the array boundary" do
    a = [1, 2, 3, 4]
    a[4] = 8
    a.should == [1, 2, 3, 4, 8]
  end
    
end

describe "Array#[]= with [index, count]" do
  it "returns non-array value if non-array value assigned" do
    a = [1, 2, 3, 4, 5]
    (a[2, 3] = 10).should == 10
  end

  it "returns array if array assigned" do
    a = [1, 2, 3, 4, 5]
    (a[2, 3] = [4, 5]).should == [4, 5]
  end

  it "removes the section defined by start, length when set to nil" do
      a = ['a', 'b', 'c', 'd', 'e']
      a[1, 3] = nil
      a.should == ["a", "e"]
    end
    
  it "removes the section when set to nil if negative index within bounds and cnt > 0" do
    a = ['a', 'b', 'c', 'd', 'e']
    a[-3, 2] = nil
    a.should == ["a", "b", "e"]
  end
  
  it "replaces the section defined by start, length to other" do
      a = [1, 2, 3, 4, 5, 6]
      a[0, 1] = 2
      a[3, 2] = ['a', 'b', 'c', 'd']
      a.should == [2, 2, 3, "a", "b", "c", "d", 6]
    end

  it "replaces the section to other if idx < 0 and cnt > 0" do
    a = [1, 2, 3, 4, 5, 6]
    a[-3, 2] = ["x", "y", "z"]
    a.should == [1, 2, 3, "x", "y", "z", 6]
  end

  it "replaces the section to other even if cnt spanning beyond the array boundary" do
    a = [1, 2, 3, 4, 5]
    a[-1, 3] = [7, 8]
    a.should == [1, 2, 3, 4, 7, 8]
  end

  it "pads the Array with nils if the span is past the end" do
    a = [1, 2, 3, 4, 5]
    a[10, 1] = [1]
    a.should == [1, 2, 3, 4, 5, nil, nil, nil, nil, nil, 1]

    b = [1, 2, 3, 4, 5]
    b[10, 0] = [1]
    a.should == [1, 2, 3, 4, 5, nil, nil, nil, nil, nil, 1]
  end

  it "inserts other section in place defined by idx" do
    a = [1, 2, 3, 4, 5]
    a[3, 0] = [7, 8]
    a.should == [1, 2, 3, 7, 8, 4, 5]

    b = [1, 2, 3, 4, 5]
    b[1, 0] = b
    b.should == [1, 1, 2, 3, 4, 5, 2, 3, 4, 5]
  end
  
  it "raises an IndexError when passed start and negative length" do
    a = [1, 2, 3, 4]
    lambda { a[-2, -1] = "" }.should raise_error(IndexError)
    lambda { a[0, -1] = ""  }.should raise_error(IndexError)
    lambda { a[2, -1] = ""  }.should raise_error(IndexError)
    lambda { a[4, -1] = ""  }.should raise_error(IndexError)
    lambda { a[10, -1] = "" }.should raise_error(IndexError)
    lambda { [1, 2, 3, 4,  5][2, -1] = [7, 8] }.should raise_error(IndexError)
  end

  it "sets elements when passed start, length" do
    a = [];   a[0, 0] = nil;            a.should == []
    a = [];   a[2, 0] = nil;            a.should == [nil, nil]
    a = [];   a[0, 2] = nil;            a.should == []
    a = [];   a[2, 2] = nil;            a.should == [nil, nil]

    a = [];   a[0, 0] = [];             a.should == []
    a = [];   a[2, 0] = [];             a.should == [nil, nil]
    a = [];   a[0, 2] = [];             a.should == []
    a = [];   a[2, 2] = [];             a.should == [nil, nil]

    a = [];   a[0, 0] = ["a"];          a.should == ["a"]
    a = [];   a[2, 0] = ["a"];          a.should == [nil, nil, "a"]
    a = [];   a[0, 2] = ["a"];          a.should == ["a"]
    a = [];   a[2, 2] = ["a"];          a.should == [nil, nil, "a"]
    
    a = [];   a[0, 0] = ["a","b"];      a.should == ["a", "b"]
    a = [];   a[2, 0] = ["a","b"];      a.should == [nil, nil, "a", "b"]
    a = [];   a[0, 2] = ["a","b"];      a.should == ["a", "b"]
    a = [];   a[2, 2] = ["a","b"];      a.should == [nil, nil, "a", "b"]

    a = [];   a[0, 0] = ["a","b","c"];  a.should == ["a", "b", "c"]
    a = [];   a[2, 0] = ["a","b","c"];  a.should == [nil, nil, "a", "b", "c"]
    a = [];   a[0, 2] = ["a","b","c"];  a.should == ["a", "b", "c"]
    a = [];   a[2, 2] = ["a","b","c"];  a.should == [nil, nil, "a", "b", "c"]
    
    a = [1, 2, 3, 4]
    a[0, 0] = [];         a.should == [1, 2, 3, 4]
    a[1, 0] = [];         a.should == [1, 2, 3, 4]
    a[-1,0] = [];         a.should == [1, 2, 3, 4]

    a = [1, 2, 3, 4]
    a[0, 0] = [8, 9, 9];  a.should == [8, 9, 9, 1, 2, 3, 4]
    a = [1, 2, 3, 4]
    a[1, 0] = [8, 9, 9];  a.should == [1, 8, 9, 9, 2, 3, 4]
    a = [1, 2, 3, 4]
    a[-1,0] = [8, 9, 9];  a.should == [1, 2, 3, 8, 9, 9, 4]
    a = [1, 2, 3, 4]
    a[4, 0] = [8, 9, 9];  a.should == [1, 2, 3, 4, 8, 9, 9]

    a = [1, 2, 3, 4]
    a[0, 1] = [9];        a.should == [9, 2, 3, 4]
    a[1, 1] = [8];        a.should == [9, 8, 3, 4]
    a[-1,1] = [7];        a.should == [9, 8, 3, 7]
    a[4, 1] = [9];        a.should == [9, 8, 3, 7, 9]

    a = [1, 2, 3, 4]
    a[0, 1] = [8, 9];     a.should == [8, 9, 2, 3, 4]
    a = [1, 2, 3, 4]
    a[1, 1] = [8, 9];     a.should == [1, 8, 9, 3, 4]
    a = [1, 2, 3, 4]
    a[-1,1] = [8, 9];     a.should == [1, 2, 3, 8, 9]
    a = [1, 2, 3, 4]
    a[4, 1] = [8, 9];     a.should == [1, 2, 3, 4, 8, 9]
    
    a = [1, 2, 3, 4]
    a[0, 2] = [8, 9];     a.should == [8, 9, 3, 4]
    a = [1, 2, 3, 4]
    a[1, 2] = [8, 9];     a.should == [1, 8, 9, 4]
    a = [1, 2, 3, 4]
    a[-2,2] = [8, 9];     a.should == [1, 2, 8, 9]
    a = [1, 2, 3, 4]
    a[-1,2] = [8, 9];     a.should == [1, 2, 3, 8, 9]
    a = [1, 2, 3, 4]
    a[4, 2] = [8, 9];     a.should == [1, 2, 3, 4, 8, 9]

    a = [1, 2, 3, 4]
    a[0, 2] = [7, 8, 9];  a.should == [7, 8, 9, 3, 4]
    a = [1, 2, 3, 4]
    a[1, 2] = [7, 8, 9];  a.should == [1, 7, 8, 9, 4]
    a = [1, 2, 3, 4]
    a[-2,2] = [7, 8, 9];  a.should == [1, 2, 7, 8, 9]
    a = [1, 2, 3, 4]
    a[-1,2] = [7, 8, 9];  a.should == [1, 2, 3, 7, 8, 9]
    a = [1, 2, 3, 4]
    a[4, 2] = [7, 8, 9];  a.should == [1, 2, 3, 4, 7, 8, 9]
    
    a = [1, 2, 3, 4]
    a[0, 2] = [1, 1.25, 1.5, 1.75, 2]
    a.should == [1, 1.25, 1.5, 1.75, 2, 3, 4]
    a[1, 1] = a[3, 1] = []
    a.should == [1, 1.5, 2, 3, 4]
    a[0, 2] = [1]
    a.should == [1, 2, 3, 4]
    a[5, 0] = [4, 3, 2, 1]
    a.should == [1, 2, 3, 4, nil, 4, 3, 2, 1]
    a[-2, 5] = nil
    a.should == [1, 2, 3, 4, nil, 4, 3]
    a[-2, 5] = []
    a.should == [1, 2, 3, 4, nil]
    a[0, 2] = nil
    a.should == [3, 4, nil]
    a[0, 100] = [1, 2, 3]
    a.should == [1, 2, 3]
    a[0, 2] *= 2
    a.should == [1, 2, 1, 2, 3]
    a[0, 2] |= [2, 3, 4]
    a.should == [1, 2, 3, 4, 1, 2, 3]
    a[2, 0] += [3, 2, 2]
    a.should == [1, 2, 3, 2, 2, 3, 4, 1, 2, 3]
    a[0, 4] -= [2, 3]
    a.should == [1, 2, 3, 4, 1, 2, 3]
    a[0, 6] &= [4]
    a.should == [4, 3]
  end
  
end

describe "Array#[]= with [m..n]" do
  it "returns non-array value if non-array value assigned" do
    a = [1, 2, 3, 4, 5]
    (a[2..4] = 10).should == 10
  end
  
  it "returns array if array assigned" do
    a = [1, 2, 3, 4, 5]
    (a[2..4] = [7, 8]).should == [7, 8]
  end
  
  it "removes the section defined by range when set to nil" do
      a = [1, 2, 3, 4, 5]
      a[0..1] = nil
      a.should == [3, 4, 5]
    end

  it "removes the section when set to nil if m and n < 0" do
    a = [1, 2, 3, 4, 5]
    a[-3..-2] = nil
    a.should == [1, 2, 5]
  end
    
  it "replaces the section defined by range" do
      a = [6, 5, 4, 3, 2, 1]
      a[1...2] = 9
      a[3..6] = [6, 6, 6]
      a.should == [6, 9, 4, 6, 6, 6]
    end

  it "replaces the section if m and n < 0" do
    a = [1, 2, 3, 4, 5]
    a[-3..-2] = [7, 8, 9]
    a.should == [1, 2, 7, 8, 9, 5]
  end

  it "replaces the section if m < 0 and n > 0" do
    a = [1, 2, 3, 4, 5]
    a[-4..3] = [8]
    a.should == [1, 8, 5]
  end

  it "inserts the other section at m if m > n" do
    a = [1, 2, 3, 4, 5]
    a[3..1] = [8]
    a.should == [1, 2, 3, 8, 4, 5]
  end
  
  it "accepts Range subclasses" do
    a = [1, 2, 3, 4]
    range_incl = ArraySpecs::MyRange.new(1, 2)
    range_excl = ArraySpecs::MyRange.new(-3, -1, true)

    a[range_incl] = ["a", "b"]
    a.should == [1, "a", "b", 4]
    a[range_excl] = ["A", "B"]
    a.should == [1, "A", "B", 4]
  end
end

describe "Array#[] after a shift" do
  it "works for insertion" do
    a = [1,2]
    a.shift
    a.shift
    a[0,0] = [3,4]
    a.should == [3,4]
  end
end

