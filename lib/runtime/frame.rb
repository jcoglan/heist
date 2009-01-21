module Heist
  class Runtime
    
    class Frame
      attr_reader :value
      
      def initialize(list, scope)
        push(list, scope)
        @stack = scope.runtime.stack
      end
      
      def evaluate
        @stack << self
        expand! while !@queue.empty?
        @stack.pop
        @value
      end
      
      def push(cell, scope = nil)
        unless List === cell
          cell = Heist.value_of(cell, scope)
          return @value = cell
        end
        @queue ||= []
        @queue << [cell, scope]
      end
      
    private
      
      def expand!
        list, scope = *@queue.shift
        function = list.first.eval(scope)
        bindings = list.rest.map { |cell| Binding.new(cell, scope) }
        # puts ". " * @stack.size + "(#{first.text_value})" if Scheme::Identifier === first
        function.call(self, scope, bindings)
      end
    end
    
  end
end

