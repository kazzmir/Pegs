require File.dirname(__FILE__) + '/../../spec_helper'
require "stringio"

describe "StringIO#gets when passed [seperator]" do
  before(:each) do
    @io = StringIO.new("this>is>an>example")
  end

  it "returns the data read till the next occurence of the passed seperator" do
    @io.gets(">").should == "this>"
    @io.gets(">").should == "is>"
    @io.gets(">").should == "an>"
    @io.gets(">").should == "example"
  end

  it "sets $_ to the read content" do
    @io.gets(">")
    $_.should == "this>"
    @io.gets(">")
    $_.should == "is>"
    @io.gets(">")
    $_.should == "an>"
    @io.gets(">")
    $_.should == "example"
    @io.gets(">")
    $_.should be_nil
  end

  it "updates self's lineno by one" do
    @io.gets(">")
    @io.lineno.should eql(1)
    
    @io.gets(">")
    @io.lineno.should eql(2)
    
    @io.gets(">")
    @io.lineno.should eql(3)
  end
  
  it "returns the next paragraph when the passed seperator is an empty String" do
    io = StringIO.new("this is\n\nan example")
    io.gets("").should == "this is\n"
    io.gets("").should == "an example"
  end
  
  it "returns the remaining content starting at the current position when passed nil" do
    io = StringIO.new("this is\n\nan example")
    io.pos = 5
    io.gets(nil).should == "is\n\nan example"
  end

  it "tries to convert the passed seperator to a String using #to_str" do
    obj = mock('to_str')
    obj.should_receive(:to_str).and_return(">")
    @io.gets(obj).should == "this>"
  end

  it "checks whether the passed seperator responds to #to_str" do
    obj = mock('method_missing to_str')
    obj.should_receive(:respond_to?).with(:to_str).any_number_of_times.and_return(true)
    obj.should_receive(:method_missing).with(:to_str).and_return(">")
    @io.gets(obj).should == "this>"
  end
end

describe "StringIO#gets when passed no argument" do
  before(:each) do
    @io = StringIO.new("this is\nan example\nfor StringIO#gets")
  end
  
  it "returns the data read till the next occurence of $/ or till eof" do
    @io.gets.should == "this is\n"
    
    begin
      old_sep, $/ = $/, " "
      @io.gets.should == "an "
      @io.gets.should == "example\nfor "
      @io.gets.should == "StringIO#gets"
    ensure
      $/ = old_sep
    end
  end

  it "sets $_ to the read content" do
    @io.gets
    $_.should == "this is\n"
    @io.gets
    $_.should == "an example\n"
    @io.gets
    $_.should == "for StringIO#gets"
    @io.gets
    $_.should be_nil
  end

  it "updates self's position" do
    @io.gets
    @io.pos.should eql(8)
    
    @io.gets
    @io.pos.should eql(19)

    @io.gets
    @io.pos.should eql(36)
  end
  
  it "updates self's lineno" do
    @io.gets
    @io.lineno.should eql(1)
    
    @io.gets
    @io.lineno.should eql(2)
    
    @io.gets
    @io.lineno.should eql(3)
  end

  it "returns nil if self is at the end" do
    @io.pos = 36
    @io.gets.should be_nil
    @io.gets.should be_nil
  end
end

describe "StringIO#gets when in write-only mode" do
  it "raises an IOError" do
    io = StringIO.new("xyz", "w")
    lambda { io.gets }.should raise_error(IOError)

    io = StringIO.new("xyz")
    io.close_read
    lambda { io.gets }.should raise_error(IOError)
  end
end