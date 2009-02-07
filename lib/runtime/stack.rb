module Heist
  class Runtime
    
    class Stack < Array
      attr_reader :continuation_index
      
      def initialize(index = nil)
        @continuation_index = index
      end
      
      def copy(keep_last = true)
        range = keep_last ? 0..-1 : 0...-1
        copy = self.class.new(last.index)
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
      
      def empty!(index, filler)
        return nil if empty?
        last.fill!(index, filler)
        value = nil
        value = process! while not empty?
        value
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
        reviver.value
      end
    end
    
  end
end

