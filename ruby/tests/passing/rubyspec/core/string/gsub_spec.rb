require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes.rb'

describe "String#gsub with pattern and replacement" do

  it "doesn't freak out when replacing ^" do
    "Text\n".gsub(/^/, ' ').should == " Text\n"
  end

  it "returns a copy of self with all occurrences of pattern replaced with replacement" do
    "hello".gsub(/[aeiou]/, '*').should == "h*ll*"

    str = "hello homely world. hah!"
    str.gsub(/\Ah\S+\s*/, "huh? ").should == "huh? homely world. hah!"

    "hello".gsub(//, ".").should == ".h.e.l.l.o."
  end
  
  it "ignores a block if supplied" do
    "food".gsub(/f/, "g") { "w" }.should == "good"
  end

  it "supports \\G which matches at the beginning of the remaining (non-matched) string" do
    str = "hello homely world. hah!"
    str.gsub(/\Gh\S+\s*/, "huh? ").should == "huh? huh? world. hah!"
  end
  
  it "supports /i for ignoring case" do
    str = "Hello. How happy are you?"
    str.gsub(/h/i, "j").should == "jello. jow jappy are you?"
    str.gsub(/H/i, "j").should == "jello. jow jappy are you?"
  end
  
  it "doesn't interpret regexp metacharacters if pattern is a string" do
    "12345".gsub('\d', 'a').should == "12345"
    '\d'.gsub('\d', 'a').should == "a"
  end
  
  it "replaces \\1 sequences with the regexp's corresponding capture" do
    str = "hello"
    
    str.gsub(/([aeiou])/, '<\1>').should == "h<e>ll<o>"
    str.gsub(/(.)/, '\1\1').should == "hheelllloo"

    str.gsub(/.(.?)/, '<\0>(\1)').should == "<he>(e)<ll>(l)<o>()"

    str.gsub(/.(.)+/, '\1').should == "o"

    str = "ABCDEFGHIJKLabcdefghijkl"
    re = /#{"(.)" * 12}/
    str.gsub(re, '\1').should == "Aa"
    str.gsub(re, '\9').should == "Ii"
    # Only the first 9 captures can be accessed in MRI
    str.gsub(re, '\10').should == "A0a0"
  end

  it "treats \\1 sequences without corresponding captures as empty strings" do
    str = "hello!"
    
    str.gsub("", '<\1>').should == "<>h<>e<>l<>l<>o<>!<>"
    str.gsub("h", '<\1>').should == "<>ello!"

    str.gsub(//, '<\1>').should == "<>h<>e<>l<>l<>o<>!<>"
    str.gsub(/./, '\1\2\3').should == ""
    str.gsub(/.(.{20})?/, '\1').should == ""
  end

  it "replaces \\& and \\0 with the complete match" do
    str = "hello!"
    
    str.gsub("", '<\0>').should == "<>h<>e<>l<>l<>o<>!<>"
    str.gsub("", '<\&>').should == "<>h<>e<>l<>l<>o<>!<>"
    str.gsub("he", '<\0>').should == "<he>llo!"
    str.gsub("he", '<\&>').should == "<he>llo!"
    str.gsub("l", '<\0>').should == "he<l><l>o!"
    str.gsub("l", '<\&>').should == "he<l><l>o!"
    
    str.gsub(//, '<\0>').should == "<>h<>e<>l<>l<>o<>!<>"
    str.gsub(//, '<\&>').should == "<>h<>e<>l<>l<>o<>!<>"
    str.gsub(/../, '<\0>').should == "<he><ll><o!>"
    str.gsub(/../, '<\&>').should == "<he><ll><o!>"
    str.gsub(/(.)./, '<\0>').should == "<he><ll><o!>"
  end

  it "replaces \\` with everything before the current match" do
    str = "hello!"
    
    str.gsub("", '<\`>').should == "<>h<h>e<he>l<hel>l<hell>o<hello>!<hello!>"
    str.gsub("h", '<\`>').should == "<>ello!"
    str.gsub("l", '<\`>').should == "he<he><hel>o!"
    str.gsub("!", '<\`>').should == "hello<hello>"
    
    str.gsub(//, '<\`>').should == "<>h<h>e<he>l<hel>l<hell>o<hello>!<hello!>"
    str.gsub(/../, '<\`>').should == "<><he><hell>"
  end

  it "replaces \\' with everything after the current match" do
    str = "hello!"
    
    str.gsub("", '<\\\'>').should == "<hello!>h<ello!>e<llo!>l<lo!>l<o!>o<!>!<>"
    str.gsub("h", '<\\\'>').should == "<ello!>ello!"
    str.gsub("ll", '<\\\'>').should == "he<o!>o!"
    str.gsub("!", '<\\\'>').should == "hello<>"
    
    str.gsub(//, '<\\\'>').should == "<hello!>h<ello!>e<llo!>l<lo!>l<o!>o<!>!<>"
    str.gsub(/../, '<\\\'>').should == "<llo!><o!><>"
  end
  
  it "replaces \\+ with the last paren that actually matched" do
    str = "hello!"
    
    str.gsub(/(.)(.)/, '\+').should == "el!"
    str.gsub(/(.)(.)+/, '\+').should == "!"
    str.gsub(/(.)()/, '\+').should == ""
    str.gsub(/(.)(.{20})?/, '<\+>').should == "<h><e><l><l><o><!>"

    str = "ABCDEFGHIJKLabcdefghijkl"
    re = /#{"(.)" * 12}/
    str.gsub(re, '\+').should == "Ll"
  end

  it "treats \\+ as an empty string if there was no captures" do
    "hello!".gsub(/./, '\+').should == ""
  end
  
  it "maps \\\\ in replacement to \\" do
    "hello".gsub(/./, '\\\\').should == '\\' * 5
  end

  it "leaves unknown \\x escapes in replacement untouched" do
    "hello".gsub(/./, '\\x').should == '\\x' * 5
    "hello".gsub(/./, '\\y').should == '\\y' * 5
  end

  it "leaves \\ at the end of replacement untouched" do
    "hello".gsub(/./, 'hah\\').should == 'hah\\' * 5
  end
  
  it "taints the result if the original string or replacement is tainted" do
    hello = "hello"
    hello_t = "hello"
    a = "a"
    a_t = "a"
    empty = ""
    empty_t = ""
    
    hello_t.taint; a_t.taint; empty_t.taint
    
    hello_t.gsub(/./, a).tainted?.should == true
    hello_t.gsub(/./, empty).tainted?.should == true

    hello.gsub(/./, a_t).tainted?.should == true
    hello.gsub(/./, empty_t).tainted?.should == true
    hello.gsub(//, empty_t).tainted?.should == true
    
    hello.gsub(//.taint, "foo").tainted?.should == false
  end

  it "tries to convert pattern to a string using to_str" do
    pattern = mock('.')
    def pattern.to_str() "." end
    
    "hello.".gsub(pattern, "!").should == "hello!"
  end

  it "raises a TypeError when pattern can't be converted to a string" do
    lambda { "hello".gsub(:woot, "x") }.should raise_error(TypeError)
    lambda { "hello".gsub(?e, "x")    }.should raise_error(TypeError)
  end
  
  it "tries to convert replacement to a string using to_str" do
    replacement = mock('hello_replacement')
    def replacement.to_str() "hello_replacement" end
    
    "hello".gsub(/hello/, replacement).should == "hello_replacement"
  end
  
  it "raises a TypeError when replacement can't be converted to a string" do
    lambda { "hello".gsub(/[aeiou]/, :woot) }.should raise_error(TypeError)
    lambda { "hello".gsub(/[aeiou]/, ?f)    }.should raise_error(TypeError)
  end
  
  it "returns subclass instances when called on a subclass" do
    StringSpecs::MyString.new("").gsub(//, "").class.should == StringSpecs::MyString
    StringSpecs::MyString.new("").gsub(/foo/, "").class.should == StringSpecs::MyString
    StringSpecs::MyString.new("foo").gsub(/foo/, "").class.should == StringSpecs::MyString
    StringSpecs::MyString.new("foo").gsub("foo", "").class.should == StringSpecs::MyString
  end

  # Note: $~ cannot be tested because mspec messes with it
  
  it "sets $~ to MatchData of last match and nil when there's none" do
    'hello.'.gsub('hello', 'x')
    $~[0].should == 'hello'
  
    'hello.'.gsub('not', 'x')
    $~.should == nil
  
    'hello.'.gsub(/.(.)/, 'x')
    $~[0].should == 'o.'
  
    'hello.'.gsub(/not/, 'x')
    $~.should == nil
  end
end

describe "String#gsub with pattern and block" do
  it "returns a copy of self with all occurrences of pattern replaced with the block's return value" do
    "hello".gsub(/./) { |s| s.succ + ' ' }.should == "i f m m p "
    "hello!".gsub(/(.)(.)/) { |*a| a.inspect }.should == '["he"]["ll"]["o!"]'
  end
  
  it "sets $~ for access from the block" do
    str = "hello"
    str.gsub(/([aeiou])/) { "<#{$~[1]}>" }.should == "h<e>ll<o>"
    str.gsub(/([aeiou])/) { "<#{$1}>" }.should == "h<e>ll<o>"
    str.gsub("l") { "<#{$~[0]}>" }.should == "he<l><l>o"
    
    offsets = []
    
    str.gsub(/([aeiou])/) do
      md = $~
      md.string.should == str
      offsets << md.offset(0)
      str
    end.should == "hhellollhello"
    
    offsets.should == [[1, 2], [4, 5]]
  end
  
  it "restores $~ after leaving the block" do
    [/./, "l"].each do |pattern|
      old_md = nil
      "hello".gsub(pattern) do
        old_md = $~
        "ok".match(/./)
        "x"
      end

      $~.should == old_md
      $~.string.should == "hello"
    end
  end

  it "sets $~ to MatchData of last match and nil when there's none for access from outside" do
    'hello.'.gsub('l') { 'x' }
    $~.begin(0).should == 3
    $~[0].should == 'l'

    'hello.'.gsub('not') { 'x' }
    $~.should == nil

    'hello.'.gsub(/.(.)/) { 'x' }
    $~[0].should == 'o.'

    'hello.'.gsub(/not/) { 'x' }
    $~.should == nil
  end

  it "raises a RuntimeError if the string is modified while substituting" do
    str = "hello"
    lambda { str.gsub(//) { str[0] = 'x' } }.should raise_error(RuntimeError)
  end
  
  it "doesn't interpolate special sequences like \\1 for the block's return value" do
    repl = '\& \0 \1 \` \\\' \+ \\\\ foo'
    "hello".gsub(/(.+)/) { repl }.should == repl
  end
  
  it "converts the block's return value to a string using to_s" do
    replacement = mock('hello_replacement')
    def replacement.to_s() "hello_replacement" end
    
    "hello".gsub(/hello/) { replacement }.should == "hello_replacement"
    
    obj = mock('ok')
    def obj.to_s() "ok" end
    
    "hello".gsub(/.+/) { obj }.should == "ok"
  end
  
  it "taints the result if the original string or replacement is tainted" do
    hello = "hello"
    hello_t = "hello"
    a = "a"
    a_t = "a"
    empty = ""
    empty_t = ""
    
    hello_t.taint; a_t.taint; empty_t.taint
    
    hello_t.gsub(/./) { a }.tainted?.should == true
    hello_t.gsub(/./) { empty }.tainted?.should == true

    hello.gsub(/./) { a_t }.tainted?.should == true
    hello.gsub(/./) { empty_t }.tainted?.should == true
    hello.gsub(//) { empty_t }.tainted?.should == true
    
    hello.gsub(//.taint) { "foo" }.tainted?.should == false
  end  
end

describe "String#gsub! with pattern and replacement" do
  it "modifies self in place and returns self" do
    a = "hello"
    a.gsub!(/[aeiou]/, '*').should equal(a)
    a.should == "h*ll*"
  end

  it "taints self if replacement is tainted" do
    a = "hello"
    a.gsub!(/./.taint, "foo").tainted?.should == false
    a.gsub!(/./, "foo".taint).tainted?.should == true
  end
  
  it "returns nil if no modifications were made" do
    a = "hello"
    a.gsub!(/z/, '*').should == nil
    a.gsub!(/z/, 'z').should == nil
    a.should == "hello"
  end
  
  compliant_on :ruby, :jruby do
    it "raises a TypeError when self is frozen" do
      s = "hello"
      s.freeze
    
      s.gsub!(/ROAR/, "x") # ok
      lambda { s.gsub!(/e/, "e")       }.should raise_error(TypeError)
      lambda { s.gsub!(/[aeiou]/, '*') }.should raise_error(TypeError)
    end
  end
end

describe "String#gsub! with pattern and block" do
  it "modifies self in place and returns self" do
    a = "hello"
    a.gsub!(/[aeiou]/) { '*' }.should equal(a)
    a.should == "h*ll*"
  end

  it "taints self if block's result is tainted" do
    a = "hello"
    a.gsub!(/./.taint) { "foo" }.tainted?.should == false
    a.gsub!(/./) { "foo".taint }.tainted?.should == true
  end
  
  it "returns nil if no modifications were made" do
    a = "hello"
    a.gsub!(/z/) { '*' }.should == nil
    a.gsub!(/z/) { 'z' }.should == nil
    a.should == "hello"
  end
  
  compliant_on :ruby, :jruby do
    it "raises a RuntimeError when self is frozen" do
      s = "hello"
      s.freeze
  
      s.gsub!(/ROAR/) { "x" } # ok
      lambda { s.gsub!(/e/) { "e" }       }.should raise_error(RuntimeError)
      lambda { s.gsub!(/[aeiou]/) { '*' } }.should raise_error(RuntimeError)
    end
  end
end
