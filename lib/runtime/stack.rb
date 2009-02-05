module Heist
  class Runtime
    
    class Stack < Array
      def copy(last = true)
        range = last ? 0..-1 : 0...-1
        self[range].map { |frame| frame.dup }
      end
      
      def <<(frame)
        super
        return evaluate unless size == 1
        begin
          evaluate
        rescue Continuation::Unwind => unwind
          unwind.call
        end
      end
      
    private
      
      def evaluate
        value = last.evaluate
        pop
        last.fill!(value) unless empty?
        value
      end
    end
    
  end
end

