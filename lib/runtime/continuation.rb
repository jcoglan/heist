module Heist
  class Runtime
    
    class Continuation < Function
      def initialize(stack)
        @stack  = stack.copy(false)
        @target = stack.last.target
        @to_s = @stack.first.to_s
      end
      
      def call(scope, cells)
        filler = Heist.value_of(cells.first, scope)
        stack = @stack.copy
        stack.fill!(@target, filler)
        Unwind.new(scope.runtime, stack)
      end
      
      def to_s
        "#<continuation #{@to_s}>"
      end
      
      class Unwind < Exception
        def initialize(runtime, stack)
          @runtime = runtime
          @stack   = stack
        end
        
        def call
          @runtime.stack = @stack
          @stack.revive!
        end
      end
    end
    
  end
end

