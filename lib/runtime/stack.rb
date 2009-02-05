module Heist
  class Runtime
    
    class Stack < Array
      def copy(last = true)
        range = last ? 0..-1 : 0...-1
        self[range].map { |frame| frame.dup }
      end
      
      def <<(frame)
        super
        value = frame.evaluate
        pop
        last.fill!(value) unless empty?
        value
      end
    end
    
  end
end

