module Heist
  class Runtime
    
    class Continuation < Function
      def initialize(stack)
        @stack  = stack.copy(false)
        @target = stack.last.target
      end
      
      def call(scope, cells)
        stack = @stack.copy
        stack.fill!(@target, cells.car)
        stack
      end
      
      def to_s
        "#<continuation>"
      end
    end
    
  end
end

