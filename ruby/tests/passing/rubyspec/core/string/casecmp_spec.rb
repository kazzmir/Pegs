require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#casecmp" do
  it "is a case-insensitive version of String#<=>" do
    "abcdef".casecmp("abcde").should == 1
    "aBcDeF".casecmp("abcdef").should == 0
    "abcdef".casecmp("abcdefg").should == -1
    "abcdef".casecmp("ABCDEF").should == 0
  end
  
  # Broken in MRI 1.8.4
  it "doesn't consider non-ascii characters equal that aren't" do
    # -- Latin-1 --
    upper_a_tilde  = "\xC3"
    upper_a_umlaut = "\xC4"
    lower_a_tilde  = "\xE3"
    lower_a_umlaut = "\xE4"

    lower_a_tilde.casecmp(lower_a_umlaut).should_not == 0
    lower_a_umlaut.casecmp(lower_a_tilde).should_not == 0
    upper_a_tilde.casecmp(upper_a_umlaut).should_not == 0
    upper_a_umlaut.casecmp(upper_a_tilde).should_not == 0
    
    # -- UTF-8 --
    upper_a_tilde  = "\xC3\x83"
    upper_a_umlaut = "\xC3\x84"
    lower_a_tilde  = "\xC3\xA3"
    lower_a_umlaut = "\xC3\xA4"
    
    lower_a_tilde.casecmp(lower_a_umlaut).should_not == 0
    lower_a_umlaut.casecmp(lower_a_tilde).should_not == 0
    upper_a_tilde.casecmp(upper_a_umlaut).should_not == 0
    upper_a_umlaut.casecmp(upper_a_tilde).should_not == 0
  end
  
  it "doesn't do case mapping for non-ascii characters" do
    # -- Latin-1 --
    upper_a_tilde  = "\xC3"
    upper_a_umlaut = "\xC4"
    lower_a_tilde  = "\xE3"
    lower_a_umlaut = "\xE4"
    
    upper_a_tilde.casecmp(lower_a_tilde).should == -1
    upper_a_umlaut.casecmp(lower_a_umlaut).should == -1
    lower_a_tilde.casecmp(upper_a_tilde).should == 1
    lower_a_umlaut.casecmp(upper_a_umlaut).should == 1

    # -- UTF-8 --
    upper_a_tilde  = "\xC3\x83"
    upper_a_umlaut = "\xC3\x84"
    lower_a_tilde  = "\xC3\xA3"
    lower_a_umlaut = "\xC3\xA4"

    upper_a_tilde.casecmp(lower_a_tilde).should == -1
    upper_a_umlaut.casecmp(lower_a_umlaut).should == -1
    lower_a_tilde.casecmp(upper_a_tilde).should == 1
    lower_a_umlaut.casecmp(upper_a_umlaut).should == 1
  end
  
  it "ignores subclass differences" do
    str = "abcdef"
    my_str = StringSpecs::MyString.new(str)
    
    str.casecmp(my_str).should == 0
    my_str.casecmp(str).should == 0
    my_str.casecmp(my_str).should == 0
  end
end
