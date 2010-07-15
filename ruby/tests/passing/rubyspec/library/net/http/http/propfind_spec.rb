require File.dirname(__FILE__) + '/../../../../spec_helper'
require 'net/http'
require File.dirname(__FILE__) + "/fixtures/http_server"

describe "Net::HTTP#propfind" do
  before(:all) do
    NetHTTPSpecs.start_server
  end
  
  after(:all) do
    NetHTTPSpecs.stop_server
  end
  
  before(:each) do
    @http = Net::HTTP.start("localhost", 3333)
  end
  
  it "sends an propfind request to the passed path and returns the response" do
    response = @http.propfind("/request", "test=test")
    response.body.should == "Request type: PROPFIND"
  end
  
  it "returns a Net::HTTPResponse" do
    @http.propfind("/request", "test=test").should be_kind_of(Net::HTTPResponse)
  end
end
