require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Kernel#callcc" do
  not_supported_on(:jruby) do
    it "is a private method" do
      Kernel.private_instance_methods.should include("callcc")
    end
    
    it "is possible to exit a loop like a break" do
      i = 0
      Kernel.callcc do |x|
        loop do
          i += 1
          x.call() if i == 5
        end
      end.should == nil
      i.should == 5
    end

    it "is possible to call a continuation multiple times" do
      i = 0
      cont = nil
      Kernel.callcc {|cont|}
      i += 1
      cont.call() if i < 5
      i.should == 5    
    end

    it "returns the results of a block if block is not called" do
      cont = nil
      a = callcc {|cont| 0}
      cont.call(1) if a == 0
      a.should == 1
    end

    it "returns the arguments to call" do
      callcc {|cont| cont.call }.should == nil
      callcc {|cont| cont.call 1 }.should == 1
      callcc {|cont| cont.call 1,2,3 }.should == [1,2,3]
    end

    it "preserves changes to block-local scope" do
      i = "before"
      cont = callcc { |c| c }
      if cont # nil the second time
        i = "after"
        cont.call
      end
      i.should == "after"
    end

    it "preserves changes to method-local scope" do
      # This spec tests that a continuation shares the same locals
      # tuple as the scope that created it.
      KernelSpecs.before_and_after.should == "after"
    end

    it "raises a LocalJumpError if callcc is not given a block" do
      lambda { Kernel.callcc }.should raise_error(LocalJumpError)
    end

  end
end

describe "Kernel.callcc" do
  it "needs to be reviewed for spec completeness"
end
