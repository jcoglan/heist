module Heist
  class Runtime
    
    class Continuation < Function
      def initialize(stack)
        @stack  = stack.copy(false)
        @target = stack.last.target
        @to_s = @stack.first.to_s
      end
      
      def call(scope, cells)
        filler = Heist.evaluate(cells.first, scope)
        stack = @stack.copy
        stack.fill!(@target, filler)
        stack
      end
      
      def to_s
        "#<continuation #{@to_s}>"
      end
    end
    
  end
end

