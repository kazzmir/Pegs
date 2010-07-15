require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/fixtures/classes'

describe "Kernel.throw" do
  it "transfers control to the end of the active catch block waiting for symbol" do
    catch(:blah) do
      :value
      throw :blah
      fail("throw didn't transfer the control")
    end.should be_nil
  end

  it "transfers control to the innermost catch block waiting for the same sympol" do
    one = two = three = 0
    catch :duplicate do
      catch :duplicate do
        catch :duplicate do
          one = 1
          throw :duplicate
        end
        two = 2
        throw :duplicate
      end
      three = 3
      throw :duplicate
    end
    [one, two, three].should == [1, 2, 3]
  end

  it "sets the return value of the catch block to nil by default" do
    res = catch :blah do
      throw :blah
    end
    res.should == nil
  end

  it "sets the return value of the catch block to a value specified as second parameter" do
    res = catch :blah do
      throw :blah, :return_value
    end
    res.should == :return_value
  end

  it "raises NameError if there is no catch block for the symbol" do
    proc {
      throw :blah
    }.should raise_error(NameError, "uncaught throw `blah'") { |error|
      # TODO:
      # See: http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-core/17629
      #
      # The ruby docs are not clear whether NameError#name should
      # retrun String or Symbol. Well, the docs state the *String*
      # should be returned, but the actual MRI behavior is to return Symbol.
      # And in MRI 1.9, even different Exception raised altogether.

      # So, instead of checking that error.name == :blah, we perform
      # more generic test, suitable for different implementations
      # (like JRuby, since JRuby follows the ruby-doc, and returns String).
      error.name.to_s.should == "blah"
    }
  end

  it "raises ArgumentError if 3 or more arguments provided" do
    lambda {
      catch :blah do
        throw :blah, :return_value, 2
      end
    }.should raise_error(ArgumentError)

    lambda {
      catch :blah do
        throw :blah, :return_value, 2, 3, 4, 5
      end
    }.should raise_error(ArgumentError)
  end

  it "raises TypeError if the first argument is not a symbol" do
    lambda {
      catch :blah do
        throw Object.new
      end
    }.should raise_error(TypeError)
  end

end

describe "Kernel#throw" do
  it "is a private method" do
    Kernel.private_instance_methods.should include("throw")
  end
end
