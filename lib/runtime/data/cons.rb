module Heist
  class Runtime
    
    class Cons
      include Enumerable
      include Expression
      
      attr_accessor :car, :cdr
      
      NULL = self.new
      NULL.car = NULL.cdr = NULL
      NULL.freeze
      
      class << self
        def construct(enum, syntax = false, &block)
          pairs = enum.map do |value|
            value = block.call(value) if block_given?
            self.new(value)
          end
          pairs.inject { |former, latter| former.cdr = latter }
          pairs.first || (syntax ? self.new : NULL)
        end
        
        alias :[] :construct
      end
      
      def initialize(car = nil, cdr = nil)
        self.car = car
        self.cdr = cdr
      end
      
      def car=(car)
        @car = car.nil? ? NULL : car
        car.parent = self if Expression === car and car != NULL
      end
      
      def cdr=(cdr)
        @cdr = cdr.nil? ? NULL : cdr
      end
      
      def each
        pair, tail = self, NULL
        while Cons === pair and pair.pair?
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
        @car != NULL
      end
      
      def improper?
        pair? and not list?
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

