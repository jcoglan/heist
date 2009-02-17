module Heist
  class Runtime
    
    class Stackless
      def <<(frame)
        @current = frame
        @current = process! while incomplete?
        @current
      end
      
    private
      
      def process!
        expression, scope = @current.expression,
                            @current.scope
        
        if Body === @current
          expression[0...-1].each { |expr| Heist.evaluate(expr, scope) }
          return Frame.new(expression.last, scope)
        end
        
        case expression
        
          when List then
            first = Heist.evaluate(expression.first, scope)
            value = first.call(scope, expression.rest)
            return value unless Macro::Expansion === value
            
            expression.replace(value.expression)
            return Frame.new(value.expression, scope)
        
          when Identifier then
            scope[expression]
        
          else
            expression
        end
      end
      
      def incomplete?
        (Frame === @current) or (Binding === @current)
      end
    end
    
  end
end

