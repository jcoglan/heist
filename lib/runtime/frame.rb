module Heist
  class Runtime
    
    class Frame
      attr_reader :value
      
      def initialize(function, scope, bindings, parent = nil)
        @function = function
        @scope    = scope
        @bindings = bindings
        @parent   = parent
        @queue    = []
      end
      
      def eval
        @function.call(self, @scope, @bindings)
        @queue.shift.eval while !@queue.empty?
      end
      
      def push(function, scope, bindings)
        @queue << Frame.new(function, scope, bindings, self)
      end
      
      def send(value)
        if @parent
          @parent.send(value)
        else
          @value = value
        end
      end
    end
    
  end
end

