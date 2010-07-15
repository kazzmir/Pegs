require File.dirname(__FILE__) + '/../fixtures/classes'

describe :io_tty, :shared => true do
  # Yeah, this will probably break.
  it "returns true if this stream is a terminal device (TTY)" do
    File.open('/dev/tty') {|f| f.send @method }.should == true
  end

  it "returns false if this stream is not a terminal device (TTY)" do
    File.open(__FILE__) {|f| f.send @method }.should == false
  end

  it "raises IOError on closed stream" do
    lambda { IOSpecs.closed_file.send @method }.should raise_error(IOError)
  end
end
