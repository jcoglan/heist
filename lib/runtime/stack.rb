module Heist
  class Runtime
    
    class Stack < Array
      def copy(last = true)
        range = last ? 0..-1 : 0...-1
        self[range].map { |frame| frame.dup }
      end
    end
    
  end
end

