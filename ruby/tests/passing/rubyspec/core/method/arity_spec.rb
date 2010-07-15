require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Method#arity" do
  before(:each) do
    @m = MethodSpecs::Methods.new
  end

  it "returns n, where n is the number of required arguments, when there are zero or more required arguments only" do
    @m.method(:zero).arity.should    == 0
    @m.method(:one_req).arity.should == 1
    @m.method(:two_req).arity.should == 2
  end

  it "returns -(n+1), where n is the number of required arguments, when there is at least one optional argument" do
    @m.method(:one_opt).arity.should         == -1
    @m.method(:one_req_one_opt).arity.should == -2
    @m.method(:one_req_two_opt).arity.should == -2
    @m.method(:two_req_one_opt).arity.should == -3
  end

  it "returns -(n+1), where n is the number of required arguments, when there is a splat argument, regardless of optional arguments" do
    @m.method(:zero_with_splat).arity.should            == -1
    @m.method(:one_req_with_splat).arity.should         == -2
    @m.method(:one_req_one_opt_with_splat).arity.should == -2
    @m.method(:one_req_two_opt_with_splat).arity.should == -2
    @m.method(:two_req_with_splat).arity.should         == -3
    @m.method(:two_req_one_opt_with_splat).arity.should == -3
  end

  it "returns the same value regardless of the presence of a block" do
    @m.method(:zero_with_block).arity.should                      == @m.method(:zero).arity
    @m.method(:one_req_with_block).arity.should                   == @m.method(:one_req).arity
    @m.method(:two_req_with_block).arity.should                   == @m.method(:two_req).arity

    @m.method(:one_opt_with_block).arity.should                   == @m.method(:one_opt).arity
    @m.method(:one_req_one_opt_with_block).arity.should           == @m.method(:one_req_one_opt).arity
    @m.method(:one_req_two_opt_with_block).arity.should           == @m.method(:one_req_two_opt).arity
    @m.method(:two_req_one_opt_with_block).arity.should           == @m.method(:two_req_one_opt).arity

    @m.method(:zero_with_splat_and_block).arity.should            == @m.method(:zero_with_splat).arity
    @m.method(:one_req_with_splat_and_block).arity.should         == @m.method(:one_req_with_splat).arity
    @m.method(:one_req_one_opt_with_splat_and_block).arity.should == @m.method(:one_req_one_opt_with_splat).arity
    @m.method(:one_req_two_opt_with_splat_and_block).arity.should == @m.method(:one_req_two_opt_with_splat).arity
    @m.method(:two_req_with_splat_and_block).arity.should         == @m.method(:two_req_with_splat).arity
    @m.method(:two_req_one_opt_with_splat_and_block).arity.should == @m.method(:two_req_one_opt_with_splat).arity
  end
end
