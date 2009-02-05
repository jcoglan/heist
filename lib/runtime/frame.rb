module Heist
  class Runtime
    
    class Frame
      def initialize(list, scope)
        @current = Binding.new(list, scope)
        @stack = scope.runtime.stack
        @holes, @index = [], 0
      end
      
      def evaluate
        @stack << self
        expand! while Binding === @current
        @stack.pop
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
        list, scope = @current.expression, @current.scope
        case list
        
        when Identifier then
          @current = @current.extract
        
        when List then
          @func = Heist.value_of(list.first, scope)
          @holes, @index = list.rest, 0
          
          unless Function === @func
            rest = @holes.map { |cell| Heist.value_of(cell, scope) }
            return @current = List.new([@func] + rest)
          end
          
          @current = @func.call(scope, @holes)
          
          return unless Macro::Expansion === @current
          list.replace(@current.expression)
          @current = Binding.new(@current.expression, scope)
        
        else
          @current = list
        end
      end
    end
    
  end
end

