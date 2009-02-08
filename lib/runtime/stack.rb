module Heist
  class Runtime
    
    class Stack < Array
      attr_reader :value
      
      def <<(frame)
        super
        while not frame.complete?
          process!
          break if unwind?
        end
        rewind! if unwind?
        @value
      end
      
      def copy(keep_last = true)
        copy = self.class.new
        range = keep_last ? 0..-1 : 0...-1
        self[range].each do |frame|
          copy[copy.size] = frame.dup
        end
        copy
      end
      
      def fill!(value, subexpr = nil)
        last.fill!(value, subexpr) unless empty?
      end
      
      def revive!
        while not empty?
          process!
          break if unwind?
        end
        rewind! if unwind?
        @value
      end
      
    private
      
      def process!
        @value = last.process!
        fill!(@value, pop.expression) if last.complete? or unwind?
      end
      
      def unwind?
        Continuation::Unwind === @value
      end
      
      def rewind!
        return unless empty?
        @value = @value.call
      end
    end
    
  end
end

