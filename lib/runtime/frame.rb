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
        
          when List then
            if Syntax === @values.first or @values.size == @expression.size
              @complete = true
              merge!
              return @data unless Function === @data.first
              result = @data.first.call(@scope, @data[1..-1])
              return result unless Macro::Expansion === result
              return reset!(result.expression, true)
            end
            
            stack = @scope.runtime.stack
            stack << Frame.new(@expression[@values.size], @scope)
        
          when Identifier then
            @complete = true
            @scope[@expression]
        
          else
            @complete = true
            Heist.evaluate(@expression, @scope)
        end
      end
      
      def dup
        copy, values = super, @values.dup
        copy.instance_eval { @values = values }
        copy
      end
      
      def fill!(subexpr, value)
        subexpr ||= @expression[@values.size]
        @values << value
        @subexprs << subexpr
      end
      
      def replaces(expression)
        @target = expression
      end
      
      def target
        @target || @expression
      end
      
      def to_s
        parts = []
        @expression.each_with_index do |cell, i|
          parts << (i < @values.size ?
              @values[i] :
          i == @values.size ?
              '[]' :
              cell.to_s)
        end
        '(' + parts * ' ' + ')'
      end
      
    private
      
      def merge!
        return @data = @values unless Syntax === @values.first
        @data = @expression.dup
        @expression.each_with_index do |expr, i|
          index = @subexprs.index(expr)
          @data[i] = @values[index] if index
        end
      end
      
      def reset!(expression, replace = false)
        @expression.replace(expression) if replace
        @expression = expression
        @values     = []
        @subexprs   = []
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
        @index == @expression.size
      end
      
      def process!
        expression = @expression[@index]
        @index += 1   # increment before evaluating the expression
                      # so that when a continuation is saved we
                      # resume from the following statement
        
        return Frame.new(expression, @scope) if @index == @expression.size
        
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

