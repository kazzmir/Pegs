require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/shared/matched_size.rb'
require 'strscan'

describe "StringScanner#matched_size" do
  it_behaves_like(:strscan_matched_size, :matched_size)
end
