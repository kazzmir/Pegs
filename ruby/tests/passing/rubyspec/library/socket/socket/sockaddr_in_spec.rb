require File.dirname(__FILE__) + '/../../../spec_helper'
require File.dirname(__FILE__) + '/../fixtures/classes'
require File.dirname(__FILE__) + '/../shared/pack_sockaddr'

describe "Socket#sockaddr_in" do
  it_behaves_like :socket_pack_sockaddr_in, :sockaddr_in
end
