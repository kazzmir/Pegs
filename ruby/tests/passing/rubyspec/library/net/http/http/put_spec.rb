require File.dirname(__FILE__) + '/../../../../spec_helper'
require 'net/http'
require File.dirname(__FILE__) + "/fixtures/http_server"

describe "Net::HTTP#put" do
  before(:all) do
    NetHTTPSpecs.start_server
  end
  
  after(:all) do
    NetHTTPSpecs.stop_server
  end
  
  before(:each) do
    @http = Net::HTTP.start("localhost", 3333)
  end
  
  it "sends an put request to the passed path and returns the response" do
    response = @http.put("/request", "test=test")
    response.body.should == "Request type: PUT"
  end
  
  it "returns a Net::HTTPResponse" do
    @http.put("/request", "test=test").should be_kind_of(Net::HTTPResponse)
  end
end