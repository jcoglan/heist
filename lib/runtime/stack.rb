module Heist
  class Runtime
    
    class Stack < Array
      def initialize(index = nil)
        @continuation_index = index
      end
      
      def copy(keep_last = true)
        range = keep_last ? 0..-1 : 0...-1
        index = keep_last ? @continuation_index : last.index
        copy = self.class.new(index)
        self[range].each do |frame|
          copy[copy.size] = frame.dup
        end
        copy
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
      
      def fill!(value)
        last.fill!(@continuation_index, value) unless empty?
      end
      
      def revive!
        return nil if empty?
        value = nil
        value = process! while not empty?
        value
      rescue Continuation::Unwind => reviver
        unwind!(reviver)
      end
      
    private
      
      def process!
        value = last.process!
        index = pop.index
        raise value if Continuation::Unwind === value
        last.fill!(index, value) unless empty?
        value
      end
      
      def unwind!(reviver)
        pop while not empty?
        reviver.call
      end
    end
    
  end
end

