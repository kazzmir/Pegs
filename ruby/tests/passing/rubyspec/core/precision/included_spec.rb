require File.dirname(__FILE__) + '/../../spec_helper'

describe "Precision.included" do
  it "raises a TypeError when a class mixed with Precision does not specify induced_from" do
    class Foo ;include Precision ;end
    lambda { Foo.induced_from(1) }.should raise_error(TypeError)
  end

  it "doesn't raise a TypeError when a class mixed with Precision specifies induced_from" do
    class Foo
      include Precision
      def self.induced_from(obj)
        # nothing
      end
    end
    lambda { Foo.induced_from(1) }.should_not raise_error(TypeError)
  end
end
