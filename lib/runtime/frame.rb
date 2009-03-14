module Heist
  class Runtime
    
    class Frame
      attr_reader :expression, :scope
      
      def initialize(expression, scope)
        reset!(expression)
        @scope = scope
      end
      
      def complete?
        @complete
      end
      
      def process!
        case @expression
        
          when Cons then
            function = @values.car
            if Syntax === function or @current.null?
              @complete = true
              raise SyntaxError.new("Invalid expression: #{@expression}") unless Function === function
              result = function.call(@scope, @values.cdr)
              return result unless Macro::Expansion === result
              return reset!(result.expression, true)
            end
            
            stack = @scope.runtime.stack
            stack << Frame.new(@current.car, @scope)
        
          when Identifier then
            @complete = true
            @scope[@expression]
        
          else
            @complete = true
            Heist.evaluate(@expression, @scope)
        end
      end
      
      def clone
        copy, values = super, @values.clone
        copy.instance_eval { @values = values }
        copy
      end
      
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
      
      def replaces(expression)
        @target = expression
      end
      
      def target
        @target || @expression
      end
      
    private
      
      def reset!(expression, replace = false)
        @expression.replace(expression) if replace
        @expression = expression
        @current    = expression
        @values     = (Cons === expression) ? expression.clone : nil
        @complete   = false
      end
    end
    
    class Body < Frame
      def initialize(expressions, scope)
        @expression  = expressions
        @scope       = scope
        @values      = []
        @index       = 0
      end
      
      def complete?
        @expression.null?
      end
      
      def process!
        expression = @expression.car
        
        # Increment before evaluating the expression so that when a
        # continuation is saved we resume from the following statement
        @expression = @expression.cdr
        
        return Frame.new(expression, @scope) if complete?
        
        stack = @scope.runtime.stack
        stack << Frame.new(expression, @scope)
        stack.value
      end
      
      def fill!(value, subexpr = nil)
        @values << value
      end
      
      def to_s
        @expression.map { |e| e.to_s } * ' '
      end
    end
    
  end
end

