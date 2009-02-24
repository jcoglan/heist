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
          limit = expression.size - 1
          expression.each_with_index do |expr, i|
            return Frame.new(expr, scope) if i == limit
            Heist.evaluate(expr, scope)
          end
        end
        
        case expression
        
          when Cons then
            return expression if expression.null?
            
            first = Heist.evaluate(expression.car, scope)
            raise SyntaxError.new("Expressions must begin with a procedure") unless Function === first
            
            value = first.call(scope, expression.cdr)
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

