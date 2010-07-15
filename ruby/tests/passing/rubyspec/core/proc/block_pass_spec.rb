require File.dirname(__FILE__) + '/../../spec_helper'

describe "Proc as a block pass argument" do
  def revivify(&b)
    b
  end

  it "remains the same object if re-vivified by the target method" do
    p = Proc.new {}
    p2 = revivify(&p)
    p.object_id.should == p2.object_id
    p.should == p2
  end

  it "remains the same object if reconstructed with Proc.new" do
    p = Proc.new {}
    p2 = Proc.new(&p)
    p.object_id.should == p2.object_id
    p.should == p2
  end
end
