describe :stringio_each_char, :shared => true do
  before(:each) do
    old_kcode, $KCODE = "UTF-8", $KCODE
    @io = StringIO.new("xyz äöü")
    $KCODE = old_kcode
  end

  it "yields each character code in turn" do
    seen = []
    @io.send(@method) { |c| seen << c }
    seen.should == ["x", "y", "z", " ", "ä", "ö", "ü"]
  end

  ruby_version_is "" ... "1.8.7" do
    it "returns nil" do
      @io.send(@method) {}.should be_nil
    end

    it "yields a LocalJumpError when passed no block" do
      lambda { @io.send(@method) }.should raise_error(LocalJumpError)
    end
  end

  ruby_version_is "1.8.7" do
    it "returns self" do
      @io.send(@method) {}.should equal(@io)
    end

    it "returns an Enumerator when passed no block" do
      enum = @io.send(@method)
      enum.instance_of?(Enumerable::Enumerator).should be_true

      seen = []
      enum.each { |c| seen << c }
      seen.should == ["x", "y", "z", " ", "ä", "ö", "ü"]
    end
  end
end

describe :stringio_each_char_not_readable, :shared => true do
  it "raises an IOError" do
    io = StringIO.new("xyz", "w")
    lambda { io.send(@method) { |b| b } }.should raise_error(IOError)

    io = StringIO.new("xyz")
    io.close_read
    lambda { io.send(@method) { |b| b } }.should raise_error(IOError)
  end
end
