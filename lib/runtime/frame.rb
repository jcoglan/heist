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
        return if cell.nil?
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
        
        first = Heist.value_of(list.first, scope)
        unless Function === first
          rest = list.rest.map { |cell| Heist.value_of(cell, scope) }
          return @value = List.new([first] + rest)
        end
        
        bindings = list.rest.map { |cell| Binding.new(cell, scope) }
        # puts ". " * @stack.size + "(#{list.first})" if Identifier === list.first
        first.call(self, scope, bindings)
      end
    end
    
  end
end

