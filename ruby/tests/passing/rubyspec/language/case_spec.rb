require File.dirname(__FILE__) + '/../spec_helper'

describe "The 'case'-construct" do
  it "evaluates the body of the when clause matching the case target expression" do
    case 1
      when 2: false
      when 1: true
    end.should == true
  end

  it "evaluates the body of the when clause whose array expression includes the case target expression" do
    case 2
      when 3, 4: false
      when 1, 2: true
    end.should == true
  end

  it "evaluates the body of the when clause whose range expression includes the case target expression" do
    case 5
      when 21..30: false
      when 1..20: true
    end.should == true
  end

  it "returns nil when no 'then'-bodies are given" do
    case "a"
      when "a"
      when "b"
    end.should == nil
  end
  
  it "evaluates the 'else'-body when no other expression matches" do
    case "c"
      when "a": 'foo'
      when "b": 'bar'
      else 'zzz'
    end.should == 'zzz'
  end
  
  it "returns nil when no expression matches and 'else'-body is empty" do
    case "c"
      when "a": "a"
      when "b": "b"
      else
    end.should == nil
  end

  it "returns 2 when a then body is empty" do
    case Object.new
    when Numeric then
      1
    when String then
      # ok
    else
      2
    end.should == 2
  end

  it "returns the statement following ':'" do
    case "a"
      when "a": 'foo'
      when "b": 'bar'
    end.should == 'foo'
  end
    
  it "returns the statement following 'then'" do
    case "a"
      when "a" then 'foo'
      when "b" then 'bar'
    end.should == 'foo'
  end
    
  it "allows mixing ':' and 'then'" do
    case "b"
      when "a": 'foo'
      when "b" then 'bar'
    end.should == 'bar'
  end
    
  it "tests classes with case equality" do
    case "a"
      when String
        'foo'
      when Symbol
        'bar'
    end.should == 'foo'
  end
  
  it "tests with matching regexps" do
    case "hello"
      when /abc/: false
      when /^hell/: true
    end.should == true
  end
  
  it "does not test with equality when given classes" do
    case :symbol.class
      when Symbol
        "bar"
      when String
        "bar"
      else
        "foo"
    end.should == "foo"
  end
  
  it "takes lists of values" do
    case 'z'
      when 'a', 'b', 'c', 'd'
        "foo" 
      when 'x', 'y', 'z'
        "bar" 
    end.should == "bar"

    case 'b'
      when 'a', 'b', 'c', 'd'
        "foo" 
      when 'x', 'y', 'z'
        "bar" 
    end.should == "foo"
  end
  
  it "expands arrays to lists of values" do
    case 'z'
      when *['a', 'b', 'c', 'd']
        "foo" 
      when *['x', 'y', 'z']
        "bar" 
    end.should == "bar"
  end

  it "takes an expanded array in addition to a list of values" do
    case 'f'
      when 'f', *['a', 'b', 'c', 'd']
        "foo" 
      when *['x', 'y', 'z']
        "bar" 
    end.should == "foo"

    case 'b'
      when 'f', *['a', 'b', 'c', 'd']
        "foo" 
      when *['x', 'y', 'z']
        "bar" 
    end.should == "foo"
  end

  it "concats arrays before expanding them" do
    a = ['a', 'b', 'c', 'd']
    b = ['f']
  
    case 'f'
      when 'f', *a|b
        "foo" 
      when *['x', 'y', 'z']
        "bar" 
    end.should == "foo"
  end
  
  it "never matches when clauses with no values" do
    case nil
      when *[]
        "foo"
    end.should == nil
  end
  
  it "lets you define a method after the case statement" do
    case (def foo; 'foo'; end; 'f')
      when 'a'
        'foo'
      when 'f'
        'bar'
    end.should == 'bar'
  end
  
  it "raises a SyntaxError when 'else' is used when no 'when' is given" do
    lambda {
      eval <<-CODE
      case 4
        else
          true
      end
      CODE
    }.should raise_error(SyntaxError)
  end

  it "raises a SyntaxError when 'else' is used before a 'when' was given" do
    lambda {
      eval <<-CODE
      case 4
        else
          true
        when 4: false
      end
      CODE
    }.should raise_error(SyntaxError)
  end

  it "supports nested case statements" do
    result = false
    case :x
    when Symbol
      case :y
      when Symbol
        result = true
      end
    end
    result.should == true
  end

  it "supports nested case statements followed by a when with a splatted array" do
    result = false
    case :x
    when Symbol
      case :y
      when Symbol
        result = true
      end
    when *[Symbol]
      result = false
    end
    result.should == true
  end

  it "supports nested case statements followed by a when with a splatted non-array" do
    result = false
    case :x
    when Symbol
      case :y
      when Symbol
        result = true
      end
    when *Symbol
      result = false
    end
    result.should == true
  end

  it "works even if there's only one when statement" do
    case 1
    when 1
      100
    end.should == 100
  end
end

describe "The 'case'-construct with no target expression" do
  it "evaluates the body of the first clause when at least one of its condition expressions is true" do
      case
        when true, false: 'foo'
      end.should == 'foo'
    end
    
  it "evaluates the body of the first when clause that is not false/nil" do
    case
      when false: 'foo'
      when 2: 'bar'
      when 1 == 1: 'baz'
    end.should == 'bar'
  
    case
      when false: 'foo'
      when nil: 'foo'
      when 1 == 1: 'bar'
    end.should == 'bar'
  end
      
  it "evaluates the body of the else clause if all when clauses are false/nil" do
    case
      when false: 'foo'
      when nil: 'foo'
      when 1 == 2: 'bar'
      else 'baz'
    end.should == 'baz'
  end
  
  it "evaluates multiple conditional expressions as a boolean disjunction" do
    case
      when true, false: 'foo'
      else 'bar'
    end.should == 'foo'

    case
      when false, true: 'foo'
      else 'bar'
    end.should == 'foo'
  end
end
