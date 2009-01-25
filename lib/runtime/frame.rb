module Heist
  class Runtime
    
    class Frame
      attr_reader :value
      
      def initialize(list, scope)
        @current = Binding.new(list, scope)
        @stack = scope.runtime.stack
      end
      
      def evaluate
        @stack << self
        expand! while Binding === @current
        @stack.pop
        @current
      end
      
    private
      
      def expand!
        list, scope = @current.expression, @current.scope
        return @current = @current.extract if Identifier === list
        return @current = list unless List === list
        
        first = Heist.value_of(list.first, scope)
        unless Function === first
          rest = list.rest.map { |cell| Heist.value_of(cell, scope) }
          return @current = List.new([first] + rest)
        end
        
        bindings = list.rest.map { |cell| Binding.new(cell, scope) }
        # puts ". " * @stack.size + "(#{list.first})" if Identifier === list.first
        @current = first.call(scope, bindings)
      end
    end
    
  end
end

