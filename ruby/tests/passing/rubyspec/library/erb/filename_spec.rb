require 'erb'
require File.dirname(__FILE__) + '/../../spec_helper'

describe "ERB#filename" do
  # TODO: why does this fail on rubinius?
  it "raises an exception if there are errors processing content" do
    filename = 'foobar.rhtml'
    erb = ERB.new('<% if true %>')   # will raise SyntaxError
    erb.filename = filename
    lambda {
      begin
        erb.result(binding)
      rescue Exception => e
        @ex = e
        raise e
      end
    }.should raise_error(SyntaxError)
    expected = filename

    @ex.message =~ /^(.*?):(\d+): /
    $1.should == expected
    $2.to_i.should == 1

    # TODO: why is this different on rubinius?
    extended_on :rubinius do
      @ex.file.should == expected
      @ex.line.should == 1
    end
  end

  # TODO: why does this fail on rubinius?
  it "uses '(erb)' as filename when filename is not set" do
    erb = ERB.new('<% if true %>')   # will raise SyntaxError
    lambda {
      begin
        erb.result(binding)
      rescue Exception => e
        @ex = e
        raise e
      end
    }.should raise_error(SyntaxError)
    expected = '(erb)'

    @ex.message =~ /^(.*?):(\d+): /
    $1.should == expected
    $2.to_i.should == 1

    # TODO: why is this different on rubinius?
    extended_on :rubinius do
      @ex.file.should == expected
      @ex.line.should == 1
    end
  end
end
