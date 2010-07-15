require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/common'

describe "YAML.dump" do
  after :each do
    File.delete $test_file if File.exist? $test_file
  end
  
  it "converts an object to YAML and write result to io when io provided" do
    File.open($test_file, 'w' ) do |io|
      YAML.dump( ['badger', 'elephant', 'tiger'], io )
    end
    YAML.load_file($test_file).should == ['badger', 'elephant', 'tiger']
  end
  
  it "returns a string containing dumped YAML when no io provided" do
    YAML.dump( :locked ).should == "--- :locked\n"
  end  
  
  it "returns the same string that #to_yaml on objects" do
    ["a", "b", "c"].to_yaml.should == YAML.dump(["a", "b", "c"])
  end

  it "dumps strings into YAML strings" do
    YAML.dump("str").should == "--- str\n"
  end

  it "dumps hashes into YAML key-values" do
    YAML.dump({ "a" => "b" }).should ==  "--- \na: b\n"
  end

  it "dumps Arrays into YAML collection" do
    YAML.dump(["a", "b", "c"]).should == "--- \n- a\n- b\n- c\n"
  end
end
