require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/../shared/constants'
require 'openssl'

describe "OpenSSL::HMAC.hexdigest" do
  include HMACConstants
  it 'returns an SHA1 hex digest' do
    cur_digest = OpenSSL::Digest::Digest.new('SHA1')
    cur_digest.hexdigest.should == HMACConstants::BlankSHA1HexDigest
    hexdigest = OpenSSL::HMAC.hexdigest(cur_digest,
                                        HMACConstants::Key,
                                        HMACConstants::Contents)
    hexdigest.should == HMACConstants::SHA1Hexdigest
  end
end

# Should add in similar specs for MD5, RIPEMD160, and SHA256
