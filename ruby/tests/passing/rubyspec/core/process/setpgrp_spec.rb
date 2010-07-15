require File.dirname(__FILE__) + '/../../spec_helper'

describe "Process.setpgrp and Process.getpgrp" do
  not_supported_on :windows do
    it "take no arguments" do
      lambda { Process.setpgrp(0) }.should raise_error(ArgumentError)
      lambda { Process.getpgrp(1) }.should raise_error(ArgumentError)
    end
  
    it "set and get the process group ID of the calling process" do
      # there are two synchronization points here:
      # One for the child to let the parent know that it has finished
      #   setting its process group;
      # and another for the parent to let the child know that it's ok to die.
      read1, write1 = IO.pipe
      read2, write2 = IO.pipe
      pid = Process.fork do
        read1.close
        write2.close
        Process.setpgrp
        write1 << Process.getpgrp
        write1.close
        read2.read(1)
        read2.close
        Process.exit!
      end
      write1.close
      read2.close
      pgid = read1.read # wait for child to change process groups
      read1.close
  
      Process.getpgid(pid).should == pgid.to_i
  
      write2 << "!"
      write2.close
    end
  
  end
  
  describe "Process.setpgrp" do
    not_supported_on :windows do
      it "returns zero" do
        Process.setpgrp.should == 0
      end
    end
  end
end