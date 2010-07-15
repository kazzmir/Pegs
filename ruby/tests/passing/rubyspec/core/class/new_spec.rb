require File.dirname(__FILE__) + '/../../spec_helper'

describe "Class.new with a block given" do
  it "uses the given block as the class' body" do
    klass = Class.new do
      def self.message
        "text"
      end

      def hello
        "hello again"
      end
    end

    klass.message.should     == "text"
    klass.new.hello.should == "hello again"
  end

  it "creates a subclass of the given superclass" do
    sc = Class.new do
      def self.body
        @body
      end
      @body = self
      def message; "text"; end
    end
    klass = Class.new(sc) do
      def self.body
        @body
      end
      @body = self
      def message2; "hello"; end
    end

    klass.body.should == klass
    sc.body.should == sc
    klass.superclass.should == sc
    klass.new.message.should == "text"
    klass.new.message2.should == "hello"
    klass.dup.body.should == klass
  end
end

describe "Class.new" do
  it "creates a new anonymous class" do
    klass = Class.new
    klass.is_a?(Class).should == true

    klass_instance = klass.new
    klass_instance.is_a?(klass).should == true
  end

  it "creates a class without a name" do
    Class.new.name.should == ""
  end

  it "creates a class that can be given a name by assigning it to a constant" do
    MyClass = Class.new
    MyClass.name.should == "MyClass"
    a = Class.new
    MyClass::NestedClass = a
    MyClass::NestedClass.name.should == "MyClass::NestedClass"
  end

  it "sets the new class' superclass to the given class" do
    top = Class.new
    Class.new(top).superclass.should == top
  end

  it "sets the new class' superclass to Object when no class given" do
    Class.new.superclass.should == Object
  end

  it "raises a TypeError when given a non-Class" do
    error_msg = /superclass must be a Class/
    lambda { Class.new("")         }.should raise_error(TypeError, error_msg)
    lambda { Class.new(1)          }.should raise_error(TypeError, error_msg)
    lambda { Class.new(:symbol)    }.should raise_error(TypeError, error_msg)
    lambda { Class.new(mock('o'))  }.should raise_error(TypeError, error_msg)
    lambda { Class.new(Module.new) }.should raise_error(TypeError, error_msg)
  end
end
