require File.dirname(__FILE__) + '/../../../../spec_helper'
require 'net/http'
require File.dirname(__FILE__) + '/fixtures/http_server'

describe "Net::HTTP#head" do
  before(:all) do
    NetHTTPSpecs.start_server
  end
  
  after(:all) do
    NetHTTPSpecs.stop_server
  end
  
  before(:each) do
    @http = Net::HTTP.start("localhost", 3333)
  end
  
  it "sends a MOVE request to the passed path and returns the response" do
    response = @http.move("/request")
    # HEAD requests have no responses
    response.body.should == "Request type: MOVE"
  end

  it "returns a Net::HTTPResponse" do
    @http.move("/request").should be_kind_of(Net::HTTPResponse)
  end
end
