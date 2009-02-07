module Heist
  class Runtime
    
    class Continuation < Function
      def initialize(stack)
        @stack = stack
      end
      
      def call(scope, cells)
        filler = Heist.value_of(cells.first, scope)
        stack = @stack.copy
        value = stack.empty!(@stack.continuation_index, filler)
        Unwind.new(value)
      end
      
      def to_s
        @stack.first.holes.to_s
      end
      
      class Unwind < Exception
        attr_reader :value
        def initialize(value)
          @value = value
        end
      end
    end
    
  end
end

