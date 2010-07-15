require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/shared/modulo'
require 'bigdecimal'

module DivmodSpecs
  def self.check_both_nan(array)
    array.length.should == 2
    array[0].nan?.should == true
    array[1].nan?.should == true
  end
  def self.check_both_bigdecimal(array)
    array.length.should == 2
    array[0].kind_of?(BigDecimal).should == true
    array[1].kind_of?(BigDecimal).should == true
  end
end

# TODO: figure out a way to do the shared specs with helpers instead
# of spec'ing a method that does not really exist
describe "BigDecimal#mod_part_of_divmod" do
  # BigDecimal#divmod[1] behaves exactly like #modulo
  before :all do
    class BigDecimal
      def mod_part_of_divmod(arg)
        divmod(arg)[1]
      end
    end
  end

  after :all do
    class BigDecimal
      undef mod_part_of_divmod
    end
  end

  it_behaves_like :bigdecimal_modulo, :mod_part_of_divmod

  it "does NOT raise ZeroDivisionError if other is zero" do
    bd6543 = BigDecimal.new("6543.21")
    bd5667 = BigDecimal.new("5667.19")
    a = BigDecimal("1.0000000000000000000000000000000000000000005")
    b = BigDecimal("1.00000000000000000000000000000000000000000005")

    bd5667.send(@method, 0).nan?.should == true
    bd5667.send(@method, BigDecimal("0")).nan?.should == true
    @zero.send(@method, @zero).nan?.should == true
  end
end

describe "BigDecimal#divmod" do

  before(:each) do
    @a = BigDecimal("42.00000000000000000001")

    @zero = BigDecimal("0")
    @zero_pos = BigDecimal("+0")
    @zero_neg = BigDecimal("-0")

    @one = BigDecimal("1")
    @mixed = BigDecimal("1.23456789")
    @pos_int = BigDecimal("2E5555")
    @neg_int = BigDecimal("-2E5555")
    @pos_frac = BigDecimal("2E-9999")
    @neg_frac = BigDecimal("-2E-9999")
    @nan = BigDecimal("NaN")
    @infinity = BigDecimal("Infinity")
    @infinity_minus = BigDecimal("-Infinity")
    @one_minus = BigDecimal("-1")
    @frac_1 = BigDecimal("1E-99999")
    @frac_2 = BigDecimal("0.9E-99999")

    @special_vals = [@infinity, @infinity_minus, @nan]
    @regular_vals = [
      @one, @mixed, @pos_int, @neg_int, @pos_frac,
      @neg_frac, @one_minus, @frac_1, @frac_2]
    @zeroes = [@zero, @zero_pos, @zero_neg]
  end

  it "divides value, returns an array" do
    res = @a.divmod(5)
    res.kind_of?(Array).should == true
  end

  it "array contains quotient and modulus as BigDecimal" do
    res = @a.divmod(5)
    DivmodSpecs::check_both_bigdecimal(res)
    res[0].should == BigDecimal('0.8E1')
    res[1].should == BigDecimal('2.00000000000000000001')

    BigDecimal('1').divmod(BigDecimal('2')).should == [0, 1]
    BigDecimal('2').divmod(BigDecimal('1')).should == [2, 0]

    BigDecimal('1').divmod(BigDecimal('-2')).should == [-1, -1]
    BigDecimal('2').divmod(BigDecimal('-1')).should == [-2, 0]

    BigDecimal('-1').divmod(BigDecimal('2')).should == [-1, 1]
    BigDecimal('-2').divmod(BigDecimal('1')).should == [-2, 0]
  end

  it "Can be reversed with * and +" do
    # Example taken from BigDecimal documentation
    a = BigDecimal.new("42")
    b = BigDecimal.new("9")
    q, m = a.divmod(b)
    c = q * b + m
    a.should == c

    values = [@one, @one_minus, BigDecimal('2'), BigDecimal('-2'),
      BigDecimal('5'), BigDecimal('-5'), BigDecimal('10'), BigDecimal('-10'),
      BigDecimal('20'), BigDecimal('-20'), BigDecimal('100'), BigDecimal('-100'),
      BigDecimal('1.23456789E10'), BigDecimal('-1.23456789E10')
    ]

    # TODO: file MRI bug:
    # BigDecimal('1').divmod(BigDecimal('3E-9'))[0] #=> 0.3E9,
    # but really should be 0.333333333E9
    ruby_bug "#206", "1.8" do #MRI's precision is very low in some cases
      values << BigDecimal('1E-10')
      values << BigDecimal('-1E-10')
      values << BigDecimal('2E55')
      values << BigDecimal('-2E55')
      values << BigDecimal('2E-5555')
      values << BigDecimal('-2E-5555')


      values_and_zeroes = values + @zeroes
      values_and_zeroes.each do |val1|
        values.each do |val2|
          res = val1.divmod(val2)
          DivmodSpecs::check_both_bigdecimal(res)
          res[0].should == ((val1/val2).floor)
          res[1].should == (val1 - res[0] * val2)
        end
      end
    end
  end

  it "properly handles special values" do
    values = @special_vals + @zeroes
    values.each do |val1|
      values.each do |val2|
        DivmodSpecs::check_both_nan(val1.divmod(val2))
      end
    end

    @special_vals.each do |val1|
      @regular_vals.each do |val2|
        DivmodSpecs::check_both_nan(val1.divmod(val2))
      end
    end

    @regular_vals.each do |val1|
      @special_vals.each do |val2|
        DivmodSpecs::check_both_nan(val1.divmod(val2))
      end
    end
  end

  it "returns an array of two NaNs if the argument is zero" do
    values = @regular_vals + @special_vals
    values.each do |val1|
      @zeroes.each do |val2|
        DivmodSpecs::check_both_nan(val1.divmod(val2))
      end
    end
  end

  it "raises TypeError if the argument cannot be coerced to BigDecimal" do
    lambda {
      @one.divmod('1')
    }.should raise_error(TypeError)
  end

end
