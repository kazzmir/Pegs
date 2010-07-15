describe :proc_call, :shared => true do
  it "invokes self" do
    Proc.new { "test!" }.send(@method).should == "test!"
    lambda { "test!" }.send(@method).should == "test!"
    proc { "test!" }.send(@method).should == "test!"
  end
  
  it "sets self's parameters to the given values" do
    Proc.new { |a, b| a + b }.send(@method, 1, 2).should == 3
    Proc.new { |*args| args }.send(@method, 1, 2, 3, 4).should == [1, 2, 3, 4]
    Proc.new { |_, *args| args }.send(@method, 1, 2, 3).should == [2, 3]
    
    lambda { |a, b| a + b }.send(@method, 1, 2).should == 3
    lambda { |*args| args }.send(@method, 1, 2, 3, 4).should == [1, 2, 3, 4]
    lambda { |_, *args| args }.send(@method, 1, 2, 3).should == [2, 3]

    proc { |a, b| a + b }.send(@method, 1, 2).should == 3
    proc { |*args| args }.send(@method, 1, 2, 3, 4).should == [1, 2, 3, 4]
    proc { |_, *args| args }.send(@method, 1, 2, 3).should == [2, 3]
  end
  
  it "sets self's single parameter to an Array of all given values" do
    [Proc.new { |x| [x] }, lambda { |x| [x] }, proc { |x| [x] }].each do |p|
      a = p.send(@method)
      a.class.should == Array
      a.should == [nil]
      
      a = p.send(@method, 1)
      a.class.should == Array
      a.should == [1]
      
      a = p.send(@method, 1, 2)
      a.class.should == Array
      a.should == [[1, 2]]
      
      a = p.send(@method, 1, 2, 3)
      a.class.should == Array
      a.should == [[1, 2, 3]]
    end
  end
end

describe :proc_call_on_proc_new, :shared => true do
  it "replaces missing arguments with nil" do
    Proc.new { |a, b| [a, b] }.send(@method).should == [nil, nil]
    Proc.new { |a, b| [a, b] }.send(@method, 1).should == [1, nil]
  end

  it "silently ignores extra arguments" do
    Proc.new { |a, b| a + b }.send(@method, 1, 2, 5).should == 3
  end

  it "auto-explodes a single Array argument" do
    p = Proc.new { |a, b| [a, b] }
    p.send(@method, 1, 2).should == [1, 2]
    p.send(@method, [1, 2]).should == [1, 2]
    p.send(@method, [1, 2, 3]).should == [1, 2]
    p.send(@method, [1, 2, 3], 4).should == [[1, 2, 3], 4]
  end
end

describe :proc_call_on_proc_or_lambda, :shared => true do
  it "raises an ArgumentError when called with too few arguments" do
    lambda { lambda { |a, b| [a, b] }.send(@method)    }.should raise_error(ArgumentError)
    lambda { lambda { |a, b| [a, b] }.send(@method, 1) }.should raise_error(ArgumentError)
    lambda { proc { |a, b| [a, b] }.send(@method)      }.should raise_error(ArgumentError)
    lambda { proc { |a, b| [a, b] }.send(@method, 1)   }.should raise_error(ArgumentError)
  end

  it "raises an ArgumentError when called with too many arguments" do
    lambda { lambda { |a, b| [a, b] }.send(@method, 1, 2, 3) }.should raise_error(ArgumentError)
    lambda { proc { |a, b| [a, b] }.send(@method, 1, 2, 3)   }.should raise_error(ArgumentError)
  end

  it "treats a single Array argument as a single argument" do
    lambda { |a| [a] }.send(@method, [1, 2]).should == [[1, 2]]
    lambda { lambda { |a, b| [a, b] }.send(@method, [1, 2]) }.should raise_error(ArgumentError)
    proc { |a| [a] }.send(@method, [1, 2]).should == [[1, 2]]
    lambda { proc { |a, b| [a, b] }.send(@method, [1, 2]) }.should raise_error(ArgumentError)
  end
end
