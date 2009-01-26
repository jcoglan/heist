require 'forwardable'

module Heist
  class Runtime
    
    class List
      include Enumerable
      
      extend Forwardable
      def_delegators(:@cells, :first, :last, :[], :each, :<<, :insert)
      
      def initialize(cells = [])
        @cells = cells
      end
      
      def eval(scope)
        Frame.new(self, scope).evaluate
      end
      
      def map(&block)
        self.class.new(@cells.map(&block))
      end
      
      def rest
        @cells[1..-1]
      end
      
      def to_a
        @cells.map { |cell| List === cell ? cell.to_a : cell }
      end
      
      def to_s
        '(' + collect { |cell| cell.to_s } * ' ' + ')'
      end
      
      alias :inspect :to_s
    end
    
  end
end

