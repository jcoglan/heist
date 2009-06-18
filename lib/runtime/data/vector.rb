module Heist
  class Runtime
    
    class Vector < Array
      def initialize(*args, &block)
        return super(*args) unless block_given?
        args.first.each_with_index { |cell, i| self[i] = block.call(cell) }
      end
      
      def freeze!
        freeze
        each { |slot| slot.freeze! if slot.respond_to?(:freeze!) }
      end
      
      def []=(index, value)
        raise ImmutableError.new("Cannot modify vector constant") if frozen?
        super
      end
      
      def to_xml(indent = 0)
        s = " " * (4 * indent)
        "#{s}<vector>\n" + map { |cell| Heist.to_xml(cell, indent + 1) } * "\n" + "\n#{s}</vector>"
      end
      
      def inspect
        '#(' + map { |cell| cell.inspect }.join(' ') + ')'
      end
      alias :to_s :inspect
    end
    
  end
end

