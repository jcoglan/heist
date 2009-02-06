module Heist
  class Runtime
    
    class Frame
      attr_reader :holes
      
      def initialize(expression, scope = nil)
        @current = (Binding === expression) ?
                   expression :
                   Binding.new(expression, scope)
        reset_holes!
      end
      
      def process!
        follow! while Binding === @current
        @current
      end
      
      def fill!(value = nil)
        return @index += 1 if value.nil?
        @holes[@index] = value
        @index += 1
      end
      
      def dup
        copy, holes = super, @holes
        copy.instance_eval { @holes = holes.dup }
        copy
      end
      
    private
      
      def follow!
        expression, scope = @current.expression, @current.scope
        case expression
        
          when Identifier then
            @current = scope[expression]
        
          when List then
            function = Heist.value_of(@holes.first, scope)
            
            unless Function === function
              rest = @holes.rest.map { |cell| Heist.value_of(cell, scope) }
              return @current = List.new([function] + rest)
            end
            
            @current = function.call(scope, @holes.rest)
            if Macro::Expansion === @current
              expression.replace(@current.expression)
              @current = Binding.new(@current.expression, scope)
            end
            reset_holes!
        
          else
            @current = expression
        end
      end
      
      def reset_holes!
        return unless Binding === @current and
                      List === @current.expression
        @holes, @index = @current.expression.dup, 0
      end
    end
    
  end
end

