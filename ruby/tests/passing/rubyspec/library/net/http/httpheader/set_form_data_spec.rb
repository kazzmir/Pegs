require File.dirname(__FILE__) + '/../../../../spec_helper'
require 'net/http'
require File.dirname(__FILE__) + "/fixtures/classes"
require File.dirname(__FILE__) + "/shared/set_form_data"

describe "Net::HTTPHeader#set_form_data" do
  it_behaves_like :net_httpheader_set_form_data, :set_form_data
end
