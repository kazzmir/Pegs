describe :net_httpheader_each_name, :shared => true do
  before(:each) do
    @headers = NetHTTPHeaderSpecs::Example.new
    @headers["My-Header"] = "test"
    @headers.add_field("My-Other-Header", "a")
    @headers.add_field("My-Other-Header", "b")
  end
  
  describe "when passed a block" do
    it "yields each header key to the passed block (keys in lower case)" do
      res = []
      @headers.send(@method) do |key|
        res << key
      end
      res.should == ["my-header", "my-other-header"]
    end
  end

  describe "when passed no block" do
    ruby_version_is "" ... "1.8.7" do
      it "raises a LocalJumpError" do
        lambda { @headers.send(@method) }.should raise_error(LocalJumpError)
      end
    end

    ruby_version_is "1.8.7" do
      it "returns an Enumerable::Enumerator" do
        enumerator = @headers.send(@method)
        enumerator.should be_kind_of(Enumerable::Enumerator)
      
        res = []
        enumerator.each do |key|
          res << key
        end
        res.should == ["my-header", "my-other-header"]
      end
    end
  end
end