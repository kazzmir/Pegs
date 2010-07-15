require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/shared/eos.rb'
require 'strscan'

describe "StringScanner#empty?" do
  it_behaves_like(:strscan_eos, :empty?)

  it "warns in verbose mode that the method is obsolete" do
    s = StringScanner.new("abc")
    begin
      old = $VERBOSE
      lambda {
        $VERBOSE = true
        s.empty?
      }.should complain(/empty?.*obsolete.*eos?/)

      lambda {
        $VERBOSE = false
        s.empty?
      }.should_not complain
    ensure
      $VERBOSE = old
    end
  end
end
