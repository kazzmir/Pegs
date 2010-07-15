require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/fixtures/array'

describe "Array literals" do
  it "[] should return a new array populated with the given elements" do
    array = [1, 'a', nil]
    array.class.should == Array
    array[0].should == 1
    array[1].should == 'a'
    array[2].should == nil
  end

  it "[] treats empty expressions as nil elements" do
    array = [0, (), 2, (), 4]
    array.should be_kind_of(Array)
    array[0].should == 0
    array[1].should == nil
    array[2].should == 2
    array[3].should == nil
    array[4].should == 4
  end
end

describe "Bareword array literal" do
  it "%w() transforms unquoted barewords into an array" do
    a = 3
    %w(a #{3+a} 3).should == ["a", '#{3+a}', "3"]
  end

  it "%W() transforms unquoted barewords into an array, supporting interpolation" do
    a = 3
    %W(a #{3+a} 3).should == ["a", '6', "3"]
  end

  it "%W() always treats interpolated expressions as a single word" do
    a = "hello world"
    %W(a b c #{a} d e).should == ["a", "b", "c", "hello world", "d", "e"]
  end

  it "treats consecutive whitespace characters the same as one" do
    %w(a  b c  d).should == ["a", "b", "c", "d"]
    %W(hello
       world).should == ["hello", "world"]
  end

  it "treats whitespace as literals characters when escaped by a backslash" do
    %w(a b\ c d e).should == ["a", "b c", "d", "e"]
    %w(a b\
c d).should == ["a", "b\nc", "d"]
    %W(a\  b\tc).should == ["a ", "b\tc"]
    %W(white\  \  \ \  \ space).should == ["white ", " ", "  ", " space"]
  end
end

describe "The unpacking splat operator (*)" do
  it "when applied to a literal nested array, unpacks its elements into the containing array" do
    [1, 2, *[3, 4, 5]].should == [1, 2, 3, 4, 5]
  end

  it "when applied to a nested referenced array, unpacks its elements into the containing array" do
    splatted_array = [3, 4, 5]
    [1, 2, *splatted_array].should == [1, 2, 3, 4, 5]
  end

  it "unpacks the start and count arguments in an array slice assignment" do
    alphabet_1 = ['a'..'z'].to_a
    alphabet_2 = alphabet_1.dup
    start_and_count_args = [1, 10]
    
    alphabet_1[1, 10] = 'a'
    alphabet_2[*start_and_count_args] = 'a'
    
    alphabet_1.should == alphabet_2
  end

  it "unpacks a literal array into arguments in a method call" do
    tester = ArraySpec::Splat.new
    tester.unpack_3args(*[1, 2, 3]).should == [1, 2, 3]
    tester.unpack_4args(1, 2, *[3, 4]).should == [1, 2, 3, 4]
    tester.unpack_4args("a", %w(b c), *%w(d e)).should == ["a", ["b", "c"], "d", "e"]
  end

  it "unpacks a referenced array into arguments in a method call" do
    args = [1, 2, 3]
    tester = ArraySpec::Splat.new
    tester.unpack_3args(*args).should == [1, 2, 3]
    tester.unpack_4args(0, *args).should == [0, 1, 2, 3]
  end
end

describe "The packing splat operator (*)" do
  
end
