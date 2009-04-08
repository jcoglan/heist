module Heist
  class Runtime
    
    class Vector < Array
      def initialize(*args, &block)
        return super(*args) unless block_given?
        args.first.each_with_index { |cell, i| self[i] = block.call(cell) }
      end
      
      def inspect
        '#(' + map { |cell| cell.inspect }.join(' ') + ')'
      end
      alias :to_s :inspect
    end
    
  end
end

