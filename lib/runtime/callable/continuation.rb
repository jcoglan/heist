module Heist
  class Runtime
    
    class Continuation < Function
      def initialize(stack)
        @stack  = stack.copy(false)
        @target = stack.last.target
      end
      
      def call(scope, cells)
        filler = Heist.evaluate(cells.first, scope)
        stack = @stack.copy
        stack.fill!(@target, filler)
        stack
      end
      
      def to_s
        "#<continuation>"
      end
    end
    
  end
end

