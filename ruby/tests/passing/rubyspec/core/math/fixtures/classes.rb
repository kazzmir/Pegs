class IncludesMath
  include Math
end

module MathSpecs
  class Float
    def initialize(value=1.0)
      @value = value
    end

    def to_f
      @value
    end
  end

  class Integer
    def to_int
      2
    end
  end
end
