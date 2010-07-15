require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/common'
require File.dirname(__FILE__) + '/shared/closed'

describe "Dir#rewind" do
  before(:each) do
    @dir = Dir.open DirSpecs.mock_dir
  end

  after(:each) do
    @dir.close
  end

  it "resets the next read to start from the first entry" do
    first   = @dir.pos
    a       = @dir.read
    b       = @dir.read
    prejmp  = @dir.pos
    ret     = @dir.rewind
    second  = @dir.pos
    c       = @dir.read

    a.should_not == b
    b.should_not == c
    c.should     == a

    second.should_not == prejmp
  end

  it "returns the Dir instance" do
    @dir.rewind.should == @dir
  end
end

describe "Dir#rewind" do
  it_behaves_like :dir_closed, :rewind
end
