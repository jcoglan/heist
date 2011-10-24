module Heist
  class Runtime
    
    # The +Stackless+ class provides a faster execution model than +Stack+,
    # as it does not provide fine-grained enough escape points to allow
    # fully for continuations. Continuations aside, +Stackless+ supports
    # all the same language features as +Stack+, including using a
    # trampoline to implement tail call optimisation.
    class Stackless

      # Returns the result of evaluating the +Expression+ in the given
      # +Frame+ object. This API probably looks a little weird; it's like
      # this for consistency with the +Stack+ API so the two can be used
      # interchangeably without changing the implementation of
      # <tt>Expression#eval</tt>.
      #
      # The expression is evaluated by repeatedly calling <tt>Function</tt>s
      # until a concrete value is returned. Calling a Scheme procedure
      # returns a +Body+ object that binds its body to the +Scope+ created
      # by calling the procedure. As functions do not evaluate themselves
      # we can turn what would be a recursive process into an iterative
      # one and optimise tail calls. This technique is known as trampolining.
      #
      def <<(frame)
        @current = frame
        @current = process! while incomplete?
        @current
      end
      
    private
      
      # Process the current +Frame+ or +Body+ on the top of the stack. This
      # method processes one such object for each call, and we call it
      # iteratively until the value of <tt>@current</tt> is a concrete value.
      #
      # The result of calling a +Function+ (or one of its subclasses) will be
      # a value (for primitives), a +Body+ or a +Frame+, or a <tt>Macro::Expansion</tt>.
      # Functions return their bodies rather than evaluating themselves to
      # allow for trampolining.
      #
      def process!
        expression, scope = @current.expression,
                            @current.scope
        
        # For function bodies, evaluate all but the last expression and
        # return the last expression (the tail call) as a new stack frame
        if Body === @current
          limit = expression.size - 1
          expression.each_with_index do |expr, i|
            return Frame.new(expr, scope) if i == limit
            Heist.evaluate(expr, scope)
          end
        end
        
        # Handle single-expression stack frames
        case expression
        
          # If the expression is a list, evaluate the first element and
          # call the resulting function with the rest of the list
          when Cons then
            first = !expression.null? && Heist.evaluate(expression.car, scope)
            raise SyntaxError.new("Invalid expression: #{expression}") unless Function === first
            value = first.call(scope, expression.cdr)
            return value unless Macro::Expansion === value
            
            # If the return value is a macro expansion, inline it and
            # set the expansion up as the next stack frame to run
            expression.replace(value.expression)
            return Frame.new(value.expression, scope)
        
          # If the expression is a Vector (unquoted if found here), duplicate
          # it and return it. We don't want to freeze unquoted vectors as this
          # stops macros working, but also we don't want to allow procedures
          # such as vector-set! to modify the parse tree.
          when Vector then
            expression.dup
        
          # If the expression is an identifier, look up its value in
          # the current scope
          when Identifier then
            scope[expression]
        
          # Otherwise, assume the expression is data and return it
          else
            expression
        end
      end
      
      # Returns +true+ if the current computation is incomplete, that is the
      # value of <tt>@current</tt> is an expression rather than a value.
      def incomplete?
        (Frame === @current) or (Binding === @current)
      end
    end
    
  end
end

