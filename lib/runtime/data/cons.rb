module Heist
  class Runtime
    
    class Cons
      include Enumerable
      attr_accessor :car, :cdr
      
      NULL = self.new
      NULL.cdr = NULL
      NULL.freeze
      
      def initialize(car = nil, cdr = nil)
        self.car = car
        self.cdr = cdr || NULL
      end
      
      class << self
        def construct(enum)
          pairs = enum.map { |value| self.new(value) }
          pairs.inject { |former, latter| former.cdr = latter }
          pairs.first || NULL
        end
        
        alias :[] :construct
      end
      
      def each
        pair, tail = self, NULL
        while Cons === pair and pair != NULL
          yield(pair.car) if block_given?
          tail = pair
          pair = pair.cdr
        end
        tail
      end
      alias :tail :each
      
      def length
        size = 0
        tail = each { |value| size += 1 }
        raise TypeError.new("Cannot get the length of an improper list") unless tail.cdr == NULL
        size
      end
      alias :size :length
      
      def list?
        tail.cdr == NULL
      end
      
      def pair?
        not @car.nil?
      end
      
      def null?
        self == NULL
      end
      
      def to_s
        strings = []
        tail = each { |value| strings << value.to_s }.cdr
        '(' + (strings * ' ') + (tail == NULL ? '' : ' . ' + tail.to_s) + ')'
      end
      alias :inspect :to_s
    end
    
  end
end

Cons = Heist::Runtime::Cons

