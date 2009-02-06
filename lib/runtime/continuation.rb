module Heist
  class Runtime
    
    class Continuation < Function
      def initialize(stack)
        @stack = stack
      end
      
      def call(scope, cells)
        filler = Heist.value_of(cells.first, scope)
        stack = @stack.copy
        value = stack.empty!(filler)
        Unwind.new(scope, self) { |scope, cells| value }
      end
      
      def to_s
        @stack.first.holes.to_s
      end
      
      class Unwind < Exception
        def initialize(scope, continuation, callback = nil, &block)
          @scope = scope
          @continuation = continuation
          @callback = callback || block
        end
        
        def call
          @callback.call(@scope, [@continuation])
        end
      end
    end
    
  end
end

