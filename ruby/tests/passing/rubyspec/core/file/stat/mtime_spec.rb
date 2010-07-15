require File.dirname(__FILE__) + '/../../../spec_helper'

describe "File::Stat#mtime" do
  before :each do
    @file = tmp('i_exist')
    File.open(@file,'w'){|f| f.write 'rubinius'}
  end

  after :each do
    File.delete(@file) if File.exist?(@file)
  end
  
  it "should be able to determine the mtime on a File::Stat object" do
    st = File.stat(@file)
    st.mtime.class.should == Time
    st.mtime.should <= Time.now
  end
end
