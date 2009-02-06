module Heist
  class Runtime
    
    class Stack < Array
      def copy(last = true)
        range = last ? 0..-1 : 0...-1
        self.class.new(self[range].map { |frame| frame.dup })
      end
      
      def <<(frame)
        super
        return process! unless size == 1
        begin
          process!
        rescue Continuation::Unwind => reviver
          unwind!(reviver)
        end
      end
      
      def empty!(filler)
        return nil if empty?
        last.fill!
        last.fill!(filler)
        value = nil
        value = process! while not empty?
        value
      end
      
    private
      
      def process!
        value = last.process!
        pop
        raise value if Continuation::Unwind === value
        last.fill!(value) unless empty?
        value
      end
      
      def unwind!(reviver)
        pop while not empty?
        self << Frame.new(reviver.call)
      end
    end
    
  end
end

