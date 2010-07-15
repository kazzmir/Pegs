require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/common'
require File.dirname(__FILE__) + '/shared/closed'

describe "Dir#close" do
  it "closes the stream and fd and returns nil" do
    # This is a bit convoluted but we are trying to ensure the file gets closed.
    # To do that, we peek to see what the next FD number is and then probe that
    # to see whether it has been closed.
    peek = IO.sysopen DirSpecs.mock_dir
    File.for_fd(peek).close

    dir = Dir.open DirSpecs.mock_dir
    File.for_fd(peek).close                   # Should be open here

    dir.close.should == nil
    lambda { File.for_fd(peek).close }.should raise_error(SystemCallError)  # And closed here
  end
end

describe "Dir#close" do
  it_behaves_like :dir_closed, :close
end
