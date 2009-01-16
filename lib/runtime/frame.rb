module Heist
  class Runtime
    
    class Frame
      attr_reader :value
      
      def initialize(function, scope, bindings)
        @queue    = [[function, scope, bindings]]
      end
      
      def eval
        expand! while !@queue.empty?
      end
      
      def push(function, scope, bindings)
        @queue << [function, scope, bindings]
      end
      
      def send(value)
        @value = value
      end
      
    private
      
      def expand!
        call = @queue.shift
        call[0].call(self, call[1], call[2])
      end
    end
    
  end
end

