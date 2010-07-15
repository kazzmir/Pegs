require File.dirname(__FILE__) + '/../../spec_helper'

require 'resolv'

describe "Resolv#getnames" do
	it 'resolves 127.0.0.1' do
		names = nil
		lambda {
			names = Resolv.getnames("127.0.0.1")
		}.should_not raise_error(Resolv::ResolvError)
		names.should_not == nil
		names.size.should > 0
	end

end
