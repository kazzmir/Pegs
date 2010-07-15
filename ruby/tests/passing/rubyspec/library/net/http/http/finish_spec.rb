require File.dirname(__FILE__) + '/../../../../spec_helper'
require 'net/http'
require File.dirname(__FILE__) + '/fixtures/http_server'

describe "Net::HTTP#finish" do
  before(:all) do
    NetHTTPSpecs.start_server
  end
  
  after(:all) do
    NetHTTPSpecs.stop_server
  end

  before(:each) do
    @http = Net::HTTP.new("localhost", 3333)
  end

  describe "when self has been started" do
    it "closes the tcp connection" do
      @http.start
      @http.finish
      @http.started?.should be_false
    end
  end

  describe "when self has not been started yet" do
    it "raises an IOError" do
      lambda { @http.finish }.should raise_error(IOError)
    end
  end
end