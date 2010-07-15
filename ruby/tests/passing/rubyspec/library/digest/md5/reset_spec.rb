require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/shared/constants'

describe "Digest::MD5#reset" do

  it 'can returns digest state to initial conditions' do
    cur_digest = Digest::MD5.new
    cur_digest.update MD5Constants::Contents
    cur_digest.digest().should_not == MD5Constants::BlankDigest
    cur_digest.reset
    cur_digest.digest().should == MD5Constants::BlankDigest
  end

end

