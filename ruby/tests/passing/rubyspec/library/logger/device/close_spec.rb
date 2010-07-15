require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/../fixtures/common'

describe "Logger::LogDevice#close" do
  before :each do
    @file_path = tmp("test_log.log")
    @log_file = File.open(@file_path, "w+")

    # Avoid testing this with STDERR, we don't want to be closing that.
    @device = Logger::LogDevice.new(@log_file)
  end

  after :each do
    @log_file.close unless @log_file.closed?
    File.unlink(@file_path) if File.exists?(@file_path)
  end

  it "closes the LogDevice's stream" do
    @device.close
    lambda { @device.write("Test") }.should raise_error
  end

  it "raises an error if it's already closed" do
    @device.close
    lambda { @device.close}.should raise_error
  end
end
