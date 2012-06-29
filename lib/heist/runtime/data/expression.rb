module Heist
  class Runtime
    
    # Classes may mix in the +Expression+ module to signal that they represent
    # sections of code that may be evaluated. This is mostly a flag module and
    # does not provide much in the way of extensibility, since the evaluator
    # needs to know how to execute all the kinds of expressions you want it to
    # deal with (for reasons such as tail recursion, expressions are not
    # responsible for evaluating themselves).
    module Expression
      attr_accessor :parent
      
      # Replaces the receiver with +expression+ in the receiver's parent
      # expression. This is used as part of the macro expansion process.
      def replace(expression)
        return unless @parent
        @parent.car = expression
        @parent.hosts(expression)
      end
      
      # Returns the result of evaluating the receiving +Expression+ in the given
      # +scope+. Works by pushing the receiver and scope onto the runtime stack
      # (could be a +Stack+ or +Stackless+), which then evaluates the expression
      # using a trampoline.
      def eval(scope)
        scope.runtime.stack << Frame.new(self, scope)
      end
    end
    
  end
end

