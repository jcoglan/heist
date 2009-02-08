module Heist
  class Runtime
    
    class Frame
      attr_reader :expression
      
      def initialize(expression, scope)
        reset!(expression)
        @scope = scope
      end
      
      def complete?
        @complete
      end
      
      def process!
        case @expression
        
          when List then
            if MetaFunction === @values.first or @values.size == @expression.size
              @complete = true
              func = @values.first
              return List.new(@values) unless Function === func
              rest = (MetaFunction === func) ? @expression.rest : @values[1..-1]
              
              result = @values.first.call(@scope, rest)
              return result unless Macro::Expansion === result
              reset!(result.expression, true)
              return process!
            end
            
            stack = @scope.runtime.stack
            stack << Frame.new(@expression[@values.size], @scope)
        
          when Identifier then
            @complete = true
            @scope[@expression]
        
          else
            @complete = true
            Heist.value_of(@expression, @scope)
        end
      end
      
      def dup
        copy, values = super, @values.dup
        copy.instance_eval { @values = values }
        copy
      end
      
      def fill!(value)
        @values << value
      end
      
    private
      
      def reset!(expression, replace = false)
        @expression.replace(expression) if replace
        @expression = expression
        @values     = []
        @complete   = false
      end
    end
    
  end
end

