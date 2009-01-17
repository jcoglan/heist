module Heist
  class Runtime
    
    class Frame
      attr_reader :value
      
      def initialize(list, scope)
        push(list, scope)
      end
      
      def eval
        expand! while !@queue.empty?
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
        function = list.cells.first.eval(scope)
        bindings = list.cells[1..-1].map { |cell| Binding.new(cell, scope) }
        function.call(self, scope, bindings)
      end
    end
    
  end
end

