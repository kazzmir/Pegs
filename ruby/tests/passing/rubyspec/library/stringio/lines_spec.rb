require File.dirname(__FILE__) + '/../../spec_helper'
require 'stringio'
require File.dirname(__FILE__) + '/shared/each'

ruby_version_is "1.8.7" do
  describe "StringIO#lines when passed a separator" do
    it_behaves_like :stringio_each_separator, :lines
  end

  describe "StringIO#lines when passed no arguments" do
    it_behaves_like :stringio_each_no_arguments, :lines
  end

  describe "StringIO#lines when self is not readable" do
    it_behaves_like :stringio_each_not_readable, :lines
  end
end
