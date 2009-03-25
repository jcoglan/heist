module Heist
  class Runtime
    
    # The +Syntax+ class is used to model built-in special forms. +Syntax+
    # functions are universally implemented in Ruby; user-defined special
    # forms are represented using +Macro+. +Syntax+ is very simple: its
    # body is a Ruby block that accepts a +Scope+ and a +Cons+ list of
    # the expression following the special form, and the +Syntax+ class
    # does not automatically evaluate any parameters. It is up to the
    # Ruby code implementing the syntax to decide what to evaluate. For
    # example, here's a couple of implementations for Scheme's <tt>(if)</tt>
    # and <tt>(set!)</tt>.
    #
    #   env = Scope.new
    #
    #   # e.g. (set! x (+ 9 4))
    #   env['set!'] = Syntax.new(env) do |scope, cells|
    #     value = Heist.evaluate(cells.cdr.car, scope)
    #     scope.set!(cells.car, value)
    #   end
    #
    #   # e.g. (if (> 6 3) 'yes 'no)
    #   env['if'] = Syntax.new(env) do |scope, cells|
    #     which = Heist.evaluate(cells.car, scope) ? cells.cdr : cells.cdr.cdr
    #     Heist.evaluate(which.car, scope)
    #   end
    #
    class Syntax < Function

      # Calls the Ruby implementation of the +Syntax+ and returns the result.
      def call(scope, cells)
        @body.call(scope, cells)
      end
      
      # Returns a string placeholder for the +Syntax+, containing its
      # name if it has one.
      def to_s
        "#<syntax:#{ @name }>"
      end
    end
    
  end
end

