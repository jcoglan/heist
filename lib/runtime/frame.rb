module Heist
  class Runtime
    
    # +Frame+ is used by the +Stack+ class to encapsulate a single +Expression+
    # bound to a +Scope+. +Frame+ objects evaluate expressions one sub-expression
    # at a time, providing fine-grained opportunity for inspecting intermediate
    # results and supporting the Scheme notion of continuations.
    #
    # The state of a +Frame+ is held in five variables: <tt>@expression</tt>,
    # <tt>@scope</tt>, <tt>@values</tt>, <tt>@current</tt> and <tt>@complete</tt>.
    # <tt>@expression</tt> is the +Expression+ object the +Frame+ is evaluating,
    # and <tt>@scope</tt> is a +Scope+ object in which to evaluate it. <tt>@values</tt>
    # is a copy of <tt>@expression</tt> that is used to store the values returned
    # by the subexpressions; it represents the partially evaluated state of the
    # expression for use when saving a +Stack+ when creating a +Continuation+.
    # <tt>@current</tt> is a pointer to the subexpression that is to be evaluated
    # next, and <tt>@complete</tt> is a boolean that indicates whether all the
    # required subexpressions have been evaluated.
    #
    # See +Stack+ for a fuller explanation of how expressions are evaluated.
    #
    class Frame
      attr_reader :expression, :scope
      
      # A +Frame+ is initialized using an +Expression+ and a +Scope+, which
      # are simply stored as instance variables. No evaluation takes place
      # upon initialization.
      def initialize(expression, scope)
        reset!(expression)
        @scope = scope
      end
      
      # Returns +true+ iff the +Frame+ has completed evaluating its expression.
      def complete?
        @complete
      end
      
      # Processes a single subexpression from the +Frame+ and returns the
      # result. If all the required subexpressions have been evaluated, we
      # call the resulting function with the arguments and return the result
      # of that call.
      def process!
        case @expression
        
          when Cons then
            # If we have evaluated all the subexpressions, or we have a
            # +Syntax+ value first in the list, we can make a function call
            # and return its result.
            function = @values.car
            if Syntax === function or @current.null?
              @complete = true
              raise SyntaxError.new("Invalid expression: #{@expression}") unless Function === function
              result = function.call(@scope, @values.cdr)
              
              # If the result of the call is a macro expansion, inline it
              # and set its expression as the new expression for the frame
              return result unless Macro::Expansion === result
              return reset!(result.expression, true)
            end
            
            # Otherwise, we still have work to do on the subexpressions, so
            # we evaluate the next one in the list.
            stack = @scope.runtime.stack
            stack << Frame.new(@current.car, @scope)
        
          # If it's a Vector (it will be unquoted if it has arrived here, copy
          # it and return it. We cannot freeze it for fear of breaking macros,
          # and we must copy it so that vector-set! cannot modify the parse tree.
          when Vector then
            @complete = true
            @expression.dup
        
          # If the expression is an +Identifier+, just look it up.
          when Identifier then
            @complete = true
            @scope[@expression]
        
          # Otherwise, the expression is just data so return it.
          else
            @complete = true
            Heist.evaluate(@expression, @scope)
        end
      end
      
      # Returns a copy of the +Frame+. The <tt>@values</tt> variable is also
      # copied by this method so that state changes do not transfer from the
      # copy to the original.
      def clone
        copy, values = super, @values.clone
        copy.instance_eval { @values = values }
        copy
      end
      
      # Fills the hole corresponding to +subexpr+ in the receiving +Frame+ with
      # +value+. When a hole is filled, the <tt>@current</tt> pointer is moved
      # to the next subexpression that needs evaluating.
      def fill!(subexpr, value)
        epair, vpair = @expression, @values
        while Cons === epair and not epair.null?
          if epair.car.equal?(subexpr)
            vpair.car = value
            @current = epair.cdr
          end
          epair, vpair = epair.cdr, vpair.cdr
        end
      end
      
      # Sets the replacement target for the receiving +Frame+ to the given
      # +expression+. When the receiving frame returns, it will fill the
      # hole corresponding to the +expression+ in the previous +Frame+ on
      # the +Stack+.
      def replaces(expression)
        @target = expression
      end
      
      # Returns the +Expression+ that is the replacement target for the
      # previous +Frame+ on the +Stack+. When the receiving +Frame+ completes,
      # the returned value will replace the return value of this method in
      # the previous +Frame+. The +target+ of a +Frame+ is its <tt>@expression</tt>
      # by default, but tail calls require us to change the target as, when
      # a tail call returns, the +Frame+ that originated it will no longer
      # by on the stack so +replaces+ is used to change the replacement
      # target for the tail call.
      def target
        @target || @expression
      end
      
    private
      
      # Resets the +Expression+ stored in the +Frame+. If the second argument
      # is given as +true+, the given +expression+ replaces the previously
      # stored expression in the source tree; this is used to inline macro
      # expansions. Also resets the <tt>@values</tt> list to be a copy of the
      # new +Expression+.
      def reset!(expression, replace = false)
        @expression.replace(expression) if replace
        @expression = expression
        @current    = expression
        @values     = (Cons === expression) ? expression.clone : nil
        @complete   = false
      end
    end
    
    # +Body+ is a subclass of +Frame+, used for evaluating +Function+ bodies.
    # Instead of providing break points between subexpressions in a single
    # expression, it provides break points between whole expressions inside
    # a Scheme procedure. (A 'break point' is a point in code execution during
    # which +Stack+ can inspect the last value returned and decide whether to
    # continue the current +Frame+ or switch to some other action.)
    class Body < Frame
      
      # A +Body+ is initialized using a +Cons+ list containing a list of
      # <tt>Expression</tt>s that make up the body of a +Function+, and a
      # +Scope+ in which these expressions are to be evaluated.
      def initialize(expressions, scope)
        @expression  = expressions
        @scope       = scope
        @values      = []
      end
      
      # Returns +true+ iff the +Body+ has evaluated all the expressions.
      def complete?
        @expression.null?
      end
      
      # Processes the next remaining +Expression+ from the +Function+ body,
      # returning the result for inspection by the +Stack+.
      def process!
        expression = @expression.car
        
        # Increment before evaluating the expression so that when a
        # continuation is saved we resume from the following statement
        @expression = @expression.cdr
        
        # Return the final expression as a +Frame+ to enable tail calls
        return Frame.new(expression, @scope) if complete?
        
        # For all non-tail calls, evaluate the expression and return the value
        stack = @scope.runtime.stack
        stack << Frame.new(expression, @scope)
        stack.value
      end
      
      # Do-nothing override of <tt>Frame#fill!</tt>. +Function+ bodies do not
      # need to remember the return value of each expression.
      def fill!(*args)
      end
    end
    
  end
end

