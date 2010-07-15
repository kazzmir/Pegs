describe :net_httpheader_each_capitalized, :shared => true do
  before(:each) do
    @headers = NetHTTPHeaderSpecs::Example.new
    @headers["my-header"] = "test"
    @headers.add_field("my-Other-Header", "a")
    @headers.add_field("My-Other-header", "b")
  end
  
  describe "when passed a block" do
    it "yields each header entry to the passed block (capitalized keys, values joined)" do
      res = []
      @headers.send(@method) do |key, value|
        res << [key, value]
      end
      res.should == [["My-Header", "test"], ["My-Other-Header", "a, b"]]
    end
  end

  describe "when passed no block" do
    ruby_version_is "" ... "1.8.7" do
      it "raises a LocalJumpError" do
        lambda { @headers.send(@method) }.should raise_error(LocalJumpError)
      end
    end

    # TODO: This should return an Enumerator and not raise an Error
    ruby_version_is "1.8.7" do
      ruby_bug "http://redmine.ruby-lang.org/issues/show/447", "1.8.7" do
        it "returns an Enumerable::Enumerator" do
          enumerator = @headers.send(@method)
          enumerator.should be_kind_of(Enumerable::Enumerator)
    
          res = []
          enumerator.each do |key|
            res << key
          end
          res.should == [["My-Header", "test"], ["My-Other-Header", "a, b"]]
        end
      end
    end
  end
end