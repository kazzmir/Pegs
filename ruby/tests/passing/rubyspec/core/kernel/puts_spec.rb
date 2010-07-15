require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Kernel#puts" do
  it "is a private method" do
    Kernel.private_instance_methods.should include("puts")
  end

  before(:each) do
    @old_stdout = $stdout
    $stdout = IO.new(2, 'w')
  end

  after(:each) do
    $stdout = @old_stdout
  end

  it "writes just a newline when given no args" do
    $stdout.should_receive(:write).with("\n")
    Kernel.puts.should == nil
  end

  it "writes nil with a newline when given nil as an arg" do
    $stdout.should_receive(:write).with("nil")
    $stdout.should_receive(:write).with("\n")
    Kernel.puts(nil).should == nil
  end

  it "calls to_s before writing non-string objects" do
    object = mock('hola')
    object.should_receive(:to_s).and_return("hola")

    $stdout.should_receive(:write).with("hola")
    $stdout.should_receive(:write).with("\n")
    Kernel.puts(object).should == nil
  end

  it "writes each arg if given several" do
    $stdout.should_receive(:write).with("1")
    $stdout.should_receive(:write).with("two")
    $stdout.should_receive(:write).with("3")
    $stdout.should_receive(:write).with("\n").exactly(3).times
    Kernel.puts(1, "two", 3).should == nil
  end

  it "flattens a nested array before writing it" do
    $stdout.should_receive(:write).with("1")
    $stdout.should_receive(:write).with("2")
    $stdout.should_receive(:write).with("3")
    $stdout.should_receive(:write).with("\n").exactly(3).times
    Kernel.puts([1, 2, [3]]).should == nil
  end

  it "writes [...] for a recursive array arg" do
    x = []
    x << 2 << x
    $stdout.should_receive(:write).with("2")
    $stdout.should_receive(:write).with("[...]")
    $stdout.should_receive(:write).with("\n").exactly(2).times
    Kernel.puts(x).should == nil
  end

  it "writes a newline after objects that do not end in newlines" do
    $stdout.should_receive(:write).with("5")
    $stdout.should_receive(:write).with("\n")
    Kernel.puts(5).should == nil
  end

  it "does not write a newline after objects that end in newlines" do
    $stdout.should_receive(:write).with("5\n")
    Kernel.puts("5\n").should == nil
  end

  it "ignores the $/ separator global" do
    $/ = ":"
    $stdout.should_receive(:write).with("5")
    $stdout.should_receive(:write).with("\n")
    Kernel.puts(5).should == nil
    $/ = "\n"
  end
end

describe "Kernel.puts" do
  it "needs to be reviewed for spec completeness"
end
