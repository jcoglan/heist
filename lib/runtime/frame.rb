module Heist
  class Runtime
    
    class Frame
      attr_reader :value
      
      def initialize(list, scope)
        push(list, scope)
        @stack = scope.runtime.stack
      end
      
      def eval
        @stack << self
        expand! while !@queue.empty?
        @stack.pop
        @value
      end
      
      def push(cell, scope)
        unless Scheme::List === cell
          cell = cell.eval(scope) if cell.respond_to?(:eval)
          return @value = cell
        end
        @queue ||= []
        @queue << [cell, scope]
      end
      
    private
      
      def expand!
        list, scope = *@queue.shift
        first, others = list.cells.first, list.cells[1..-1]
        function = first.eval(scope)
        function = function.eval if Binding === function
        bindings = others.map { |cell| Binding.create(cell, scope) }
        # puts ".  " * @stack.size + "(#{first.text_value})" if Scheme::Identifier === first
        function.call(self, scope, bindings)
      end
    end
    
  end
end

