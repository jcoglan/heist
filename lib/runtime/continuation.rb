module Heist
  class Runtime
    
    class Continuation < MetaFunction
      def initialize(stack)
        @stack = stack
      end
      
      def to_s
        @stack.first.holes.to_s
      end
      
      class Unwind < Exception
        def initialize(scope, callback)
          @scope = scope
          stack = scope.runtime.stack.copy(false)
          @continuation = Continuation.new(stack)
          @callback = callback
        end
        
        def call
          @callback.call(@scope, [@continuation])
        end
      end
    end
    
  end
end

