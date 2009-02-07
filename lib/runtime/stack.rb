module Heist
  class Runtime
    
    class Stack < Array
      attr_reader :value
      
      def <<(frame)
        super
        while not frame.complete?
          @value = frame.process!
          break if Continuation::Unwind === @value
        end
        pop if frame.complete?
        @value
      end
    end
    
  end
end

