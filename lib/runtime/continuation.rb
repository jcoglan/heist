module Heist
  class Runtime
    
    class Continuation < Function
      def initialize(stack)
        @stack = stack
        @to_s = @stack.first.to_s
        puts "\n\nSAVED STACK\n" + stack.map { |f| f.expression.to_s } * "\n" + "\n------------------\n\n"
      end
      
      def call(scope, cells)
        filler = Heist.value_of(cells.first, scope)
        stack = @stack.copy
        stack.fill!(filler)
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

