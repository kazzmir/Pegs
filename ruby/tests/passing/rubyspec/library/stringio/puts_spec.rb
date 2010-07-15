require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "StringIO#puts when passed [Array, ...]" do
  before(:each) do
    @io = StringIO.new
  end
  
  it "writes each element of the passed Array to self, seperated by a newline" do
    @io.puts([1, 2, 3, 4])
    @io.string.should == "1\n2\n3\n4\n"
  
    @io.puts([1, 2], [3, 4])
    @io.string.should == "1\n2\n3\n4\n1\n2\n3\n4\n"
  end
  
  it "flattens nested Arrays" do
    @io.puts([1, [2, [3, [4]]]])
    @io.string.should == "1\n2\n3\n4\n"
  end
  
  it "handles self-recursive arrays correctly" do
    (ary = [5])
    ary << ary
    @io.puts(ary)
    @io.string.should == "5\n[...]\n"
  end

  it "does not honor the global output record seperator $\\" do
    begin
      old_rs, $\ = $\, "test"
      @io.puts([1, 2, 3, 4])
      @io.string.should == "1\n2\n3\n4\n"
    ensure
      $\ = old_rs
    end
  end
  
  it "first tries to convert each Array element to an Array using #to_ary" do
    obj = mock("Object")
    obj.should_receive(:to_ary).and_return(["to_ary"])
    @io.puts([obj])
    @io.string.should == "to_ary\n"
  end
  
  it "then tries to convert each Array element to a String using #to_s" do
    obj = mock("Object")
    obj.should_receive(:to_s).and_return("to_s")
    @io.puts([obj])
    @io.string.should == "to_s\n"
  end
  
  it "checks each Array element whether it responds to #to_ary" do
    obj = mock('method_missing to_ary')
    obj.should_receive(:respond_to?).with(:to_ary).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_ary).and_return(["to_ary"])
    @io.puts([obj])
    @io.string.should == "to_ary\n"
  end

  it "does not check each Array element whether it responds to #to_s if it does not respond to #to_ary" do
    obj = mock('method_missing to_s')
    obj.should_receive(:respond_to?).with(:to_ary).any_number_of_times.and_return(false)
    obj.should_not_receive(:respond_to?).with(:to_s)
    @io.puts([obj])
  end
end

describe "StringIO#puts when passed [Object, ...]" do
  before(:each) do
    @io = StringIO.new
  end

  it "does not honor the global output record seperator $\\" do
    begin
      old_rs, $\ = $\, "test"
      @io.puts(1, 2, 3, 4)
      @io.string.should == "1\n2\n3\n4\n"
    ensure
      $\ = old_rs
    end
  end

  it "does not put a \\n after each Objects that end in a newline" do
    @io.puts("1\n", "2\n", "3\n")
    @io.string.should == "1\n2\n3\n"
  end
  
  it "first tries to convert each Object to an Array using #to_ary" do
    obj = mock("Object")
    obj.should_receive(:to_ary).and_return(["to_ary"])
    @io.puts(obj)
    @io.string.should == "to_ary\n"
  end
  
  it "then tries to convert each Object to a String using #to_s" do
    obj = mock("Object")
    obj.should_receive(:to_s).and_return("to_s")
    @io.puts(obj)
    @io.string.should == "to_s\n"
  end
  
  it "checks each Object whether it responds to #to_ary" do
    obj = mock('method_missing to_ary')
    obj.should_receive(:respond_to?).with(:to_ary).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_ary).and_return(["to_ary"])
    @io.puts(obj)
    @io.string.should == "to_ary\n"
  end

  it "does not check each Object whether it responds to #to_s if it does not respond to #to_ary" do
    obj = mock('method_missing to_s')
    obj.should_receive(:respond_to?).with(:to_ary).any_number_of_times.and_return(false)
    obj.should_not_receive(:respond_to?).with(:to_s)
    @io.puts(obj)
  end
end

describe "StringIO#puts when passed no arguments" do
  before(:each) do
    @io = StringIO.new
  end

  it "returns nil" do
    @io.puts.should be_nil
  end

  it "prints a newline" do
    @io.puts
    @io.string.should == "\n"
  end
  
  it "does not honor the global output record seperator $\\" do
    begin
      old_rs, $\ = $\, "test"
      @io.puts
      @io.string.should == "\n"
    ensure
      $\ = old_rs
    end
  end
end

describe "StringIO#puts when in append mode" do
  before(:each) do
    @io = StringIO.new("example", "a")
  end

  it "appends the passed argument to the end of self" do
    @io.puts(", just testing")
    @io.string.should == "example, just testing\n"
    
    @io.puts(" and more testing")
    @io.string.should == "example, just testing\n and more testing\n"
  end

  it "correctly updates self's position" do
    @io.puts(", testing")
    @io.pos.should eql(17)
  end
end

describe "StringIO#puts when self is not writable" do
  it "raises an IOError" do
    io = StringIO.new("test", "r")
    lambda { io.puts }.should raise_error(IOError)

    io = StringIO.new("test")
    io.close_write
    lambda { io.puts }.should raise_error(IOError)
  end
end