module Heist
  class Runtime
    
    class Frame
      def initialize(list, scope)
        @current = Binding.new(list, scope)
      end
      
      def evaluate
        expand! while Binding === @current
        @current
      end
      
      def fill!(value)
        @holes[@index] = value
        @index += 1
      end
      
      def dup
        copy, holes = super, @holes
        copy.instance_eval { @holes = holes.dup }
        copy
      end
      
    private
      
      def expand!
        expression, scope = @current.expression, @current.scope
        case expression
        
          when Identifier then
            @current = scope[expression]
        
          when List then
            @holes, @index = expression.dup, 0
            function = Heist.value_of(expression.first, scope)
            
            unless Function === function
              rest = @holes.rest.map { |cell| Heist.value_of(cell, scope) }
              return @current = List.new([function] + rest)
            end
            
            @current = function.call(scope, @holes.rest)
            return unless Macro::Expansion === @current
            
            expression.replace(@current.expression)
            @current = Binding.new(@current.expression, scope)
        
          else
            @current = expression
        end
      end
    end
    
  end
end

