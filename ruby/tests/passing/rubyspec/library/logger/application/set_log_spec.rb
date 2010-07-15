require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/../fixtures/common'

describe "Logger::Application#set_log"do
  before :each do
    @file_path = tmp("test_log.log")
    @log_file = File.open(@file_path, "w+")
    @app = LoggerSpecs::TestApp.new("TestApp", @log_file)
  end

  after :each do
    @log_file.close unless @log_file.closed?
    File.unlink(@file_path) if File.exists?(@file_path)
  end

  it "sets the log device for the logger" do
    regex = /STDERR Message/
    @app.set_log(STDERR)
    lambda { @app.log(Logger::WARN, "STDERR Message") }.should output_to_fd(regex, STDERR)
  end
end
