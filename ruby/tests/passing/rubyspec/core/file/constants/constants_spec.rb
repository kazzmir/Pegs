require File.dirname(__FILE__) + '/../../../spec_helper'

["APPEND", "CREAT", "EXCL", "FNM_CASEFOLD",
  "FNM_DOTMATCH", "FNM_NOESCAPE", "FNM_PATHNAME",
  "FNM_SYSCASE", "LOCK_EX", "LOCK_NB", "LOCK_SH",
  "LOCK_UN", "NONBLOCK", "RDONLY",
  "RDWR", "SYNC", "TRUNC", "WRONLY"].each do |const|
  describe "File::Constants::#{const}" do
    it "is defined" do
      File::Constants.const_defined?(const).should be_true
    end
  end
end

platform_is :windows do
  describe "File::Constants::BINARY" do
    it "is defined" do
      File::Constants.const_defined?(:BINARY).should be_true
    end
  end
end

platform_is_not :windows do
  describe "File::Constants::NOCTTY" do
    it "is defined" do
      File::Constants.const_defined?(:NOCTTY).should be_true
    end
  end
end
