require File.dirname(__FILE__) + '/../../../spec_helper'
require 'mathn'

describe "Integer::from_prime_division" do
  it "Reverse a prime factorization of an integer" do
    Integer.from_prime_division([[2, 1], [3, 2], [7, 1]]).should == 126
  end
end
