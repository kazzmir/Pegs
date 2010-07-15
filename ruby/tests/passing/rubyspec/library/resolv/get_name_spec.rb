require File.dirname(__FILE__) + '/../../spec_helper'

require 'resolv'

describe "Resolv#getname" do
	it 'resolves 127.0.0.1' do
		lambda {
			Resolv.getname("127.0.0.1")
		}.should_not raise_error(Resolv::ResolvError)
		lambda {
			Resolv.getname("should.raise.error")
		}.should raise_error(Resolv::ResolvError)
	end

end
