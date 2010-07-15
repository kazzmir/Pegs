require File.dirname(__FILE__) + '/../../../../spec_helper'
require 'net/http'
require File.dirname(__FILE__) + '/fixtures/http_server'

describe "Net::HTTP.get_response" do
  before(:all) do
    NetHTTPSpecs.start_server
  end
  
  after(:all) do
    NetHTTPSpecs.stop_server
  end
  
  describe "when passed URI" do
    it "returns the response for the specified uri" do
      res = Net::HTTP.get_response(URI.parse('http://localhost:3333/'))
      res.content_type.should == "text/plain"
      res.body.should == "This is the index page."
    end
  end
  
  describe "when passed host, path, port" do
    it "returns the response for the specified host-path-combination" do
      res = Net::HTTP.get_response('localhost', "/", 3333)
      res.content_type.should == "text/plain"
      res.body.should == "This is the index page."
    end
  end
end
