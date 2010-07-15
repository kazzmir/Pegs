module ModuleSpecs
  CONST = :plain_constant

  class Subclass < Module
  end

  class SubclassSpec
  end

  module LookupModInMod
    INCS = :ethereal
  end

  module LookupMod
    include LookupModInMod

    MODS = :rockers
  end

  class Lookup
    include LookupMod
    LOOKIE = :lookie
  end

  class LookupChild < Lookup
  end

  class Parent
    # For private_class_method spec
    def self.private_method; end
    private_class_method :private_method

    def undefed_method() end
    undef_method :undefed_method

    # For public_class_method spec
    private
    def self.public_method; end
    public_class_method :public_method

    public
    def public_parent() end

    protected
    def protected_parent() end

    private
    def private_parent() end
  end

  module Basic
    def public_module() end

    protected
    def protected_module() end

    private
    def private_module() end
  end

  module Super
    include Basic

    def public_super_module() end

    protected
    def protected_super_module() end

    private
    def private_super_module() end

    class SuperChild
    end
  end

  module Internal
  end

  class Child < Parent
    include Super

    class << self
      include Internal
    end
    attr_accessor :accessor_method

    def public_child() end

    protected
    def protected_child() end

    private
    def private_child() end
  end

  class Child2 < Parent
    attr_reader :foo
  end

  # Be careful touching the Counts* classes as there used for testing
  # private_instance_methods, public_instance_methods, etc.  So adding, removing
  # a method will break those tests.
  module CountsMixin
    def public_3; end
    public :public_3

    def private_3; end
    private :private_3

    def protected_3; end
    protected :protected_3
  end

  class CountsParent
    include CountsMixin

    def public_2; end

    private
    def private_2; end

    protected
    def protected_2; end
  end

  class CountsChild < CountsParent
    def public_1; end

    private
    def private_1; end

    protected
    def protected_1; end
  end

  module AddConstant
  end

  module A
    CONSTANT_A = :a
    OVERRIDE = :a
    def ma(); :a; end
    def self.cma(); :a; end
  end

  module B
    CONSTANT_B = :b
    OVERRIDE = :b
    include A
    def mb(); :b; end
    def self.cmb(); :b; end
  end

  class C
    OVERRIDE = :c
    include B
  end

  module Z
    MODULE_SPEC_TOPLEVEL_CONSTANT = 1
  end

  module Alias
    def report() :report end
    alias publish report
  end

  class Allonym
    include ModuleSpecs::Alias
  end

  class Aliasing
    def self.make_alias(*a)
      alias_method(*a)
    end

    def public_one; 1; end

    def public_two(n); n * 2; end

    private
    def private_one; 1; end

    protected
    def protected_one; 1; end
  end

  module ReopeningModule
    def foo; true; end
    module_function :foo
    private :foo
  end

  # Yes, we want to re-open the module
  module ReopeningModule
    alias :foo2 :foo
    module_function :foo2
  end

  module Nesting
    @tests = {}
    def self.[](name); @tests[name]; end
    def self.[]=(name, val); @tests[name] = val; end
    def self.meta; class << self; self; end; end

    Nesting[:basic] = Module.nesting

    module ::ModuleSpecs
      Nesting[:open_first_level] = Module.nesting
    end

    class << self
      Nesting[:open_meta] = Module.nesting
    end

    def self.called_from_module_method
      Module.nesting
    end

    class NestedClass
      Nesting[:nest_class] = Module.nesting

      def self.called_from_class_method
        Module.nesting
      end

      def called_from_inst_method
        Module.nesting
      end
    end

  end

  Nesting[:first_level] = Module.nesting

  module InstanceMethMod
    def bar(); :bar; end
  end

  class InstanceMeth
    include InstanceMethMod
    def foo(); :foo; end
  end

  class InstanceMethChild < InstanceMeth
  end

  module ClassVars
    class A
      @@a_cvar = :a_cvar
    end

    module M
      @@m_cvar = :m_cvar
    end

    class B < A
      include M

      @@b_cvar = :b_cvar
    end
  end

  class CVars
    @@cls = :class

    class << self
      def cls
        @@cls
      end
      @@meta = :meta
    end

    def self.meta
      @@meta
    end

    def meta
      @@meta
    end
  end

  module MVars
    @@mvar = :mvar
  end

  class SubModule < Module
    attr_reader :special
    def initialize
      @special = 10
    end
  end

  module MA; end
  module MB
    include MA
  end
  module MC; end

  class MultipleIncludes
    include MB
  end
end

ModuleSpecs::Nesting[:root_level] = Module.nesting

class TopLevelConst
end

module AutoLoadSubject
  def self.message; "failure"; end
end

autoload :ModuleSpecAutoloadToplevel, File.join(File.dirname(__FILE__), "autoload_toplevel.rb")
