module Heist
  class Runtime
    
    class Stack < Array
      attr_reader :value
      
      def <<(frame)
        super
        while not frame.complete?
          process!
          break if @unwind
        end
        revive!(size - 1) if @tail
        rewind! if @unwind
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
      
      def fill!(subexpr, value)
        return self[size] = value if Frame === value
        last.fill!(subexpr, value) unless empty?
      end
      
      def revive!(limit = 0)
        process! while size > limit
        rewind! if @unwind
        @value
      end
      
    private
      
      def process!
        self.value = last.process! unless @unwind
        return unless last.complete? or @unwind
        @value.replaces(last.target) if @tail
        fill!(pop.target, @value)
      end
      
      def rewind!
        return unless empty?
        self.value = @value.call
      end
      
      def value=(value)
        @value  = value
        @unwind = (Continuation::Unwind === @value)
        @tail   = (Frame === @value)
      end
    end
    
  end
end

