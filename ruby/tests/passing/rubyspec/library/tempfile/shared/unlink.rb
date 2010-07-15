describe :tempfile_unlink, :shared => true do
  before(:each) do
    @tempfile = Tempfile.new("specs")
  end

  after(:each) do
    @tempfile.close
  end

  ruby_bug "", "1.8.6" do
    it "unlinks self" do
      File.should_receive(:unlink).with(@tempfile.path)
      @tempfile.send(@method)
    end
  end
end
