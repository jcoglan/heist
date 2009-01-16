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
        cell, scope = *@queue.shift
        function, bindings = *cell.bindings(scope)
        function.call(self, scope, bindings)
      end
    end
    
  end
end

