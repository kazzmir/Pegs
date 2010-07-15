require File.dirname(__FILE__) + '/../../spec_helper'

describe "Fixnum#to_sym" do
  not_compliant_on :rubinius do
    it "returns the symbol whose integer value is self" do
      a = :@sym
      b = :@ruby
      c = :@rubinius

      a.to_i.to_sym.should == :@sym
      b.to_i.to_sym.should == :@ruby
      c.to_i.to_sym.should == :@rubinius
    end

    it "returns nil if there is no symbol in the symbol table with this value" do
      100000000.to_sym.should == nil
    end

  end
end
