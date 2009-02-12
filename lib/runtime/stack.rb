module Heist
  class Runtime
    
    class Stack < Array
      attr_reader :value
      
      def <<(frame)
        super
        clear!(size - 1)
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
        return @value = value if empty?
        last.fill!(subexpr, value)
      end
      
      def clear!(limit = 0)
        process! while size > limit
        @value
      rescue Exception => ex
        restack!
        raise ex
      end
      
    private
      
      def process!
        self.value = last.process!
        return if empty? or @unwind or not last.complete?
        @value.replaces(last.target) if @tail
        fill!(pop.target, @value)
      end
      
      def value=(value)
        @value  = value
        @unwind = (Stack === @value)
        @tail   = (Frame === @value)
        restack!(value) if @unwind
      end
      
      def restack!(stack = [])
        pop while not empty?
        stack.each_with_index { |frame, i| self[i] = frame }
        @value = stack.value if Stack === stack
      end
    end
    
  end
end

