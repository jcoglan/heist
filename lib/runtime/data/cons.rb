module Heist
  class Runtime
    
    class Cons
      include Enumerable
      include Expression
      
      attr_accessor :car, :cdr
      
      NULL = self.new
      NULL.car = NULL.cdr = NULL
      def NULL.==(other); equal?(other); end
      NULL.freeze
      
      ACCESSORS = (1..4).map { |n|
        (0...(2**n)).map do |x|
          'c' + ("%0#{n}d" % x.to_s(2)).gsub('0', 'a').gsub('1', 'd') + 'r'
        end
      }.flatten
      
      class Splice
        attr_reader :cons
        def initialize(value)
          @cons = value
        end
      end
      
      class << self
        def construct(enum, &block)
          root, last = nil, nil
          enum.each do |value|
            value = block.call(value) if block_given?
            pair = (Splice === value) ? value.cons : self.new(value)
            root ||= pair
            last.cdr = pair if last
            last = pair.tail
          end
          root || NULL
        end
        alias :[] :construct
        
        def splice(expression, scope)
          value = Heist.evaluate(expression, scope)
          raise TypeError.new("(unquote-splicing) expected list, got: #{expression} -> #{value}") unless Heist.list?(value)
          Splice.new(value)
        end
      end
      
      def initialize(car = nil, cdr = nil)
        self.car = car
        self.cdr = cdr
      end
      
      ACCESSORS[2..-1].each do |accsr|
        class_eval <<-EOS
          def #{ accsr }
            #{ accsr.scan(/[ad]/).reverse.map { |s| "c#{s}r" } * '.' }
          end
        EOS
      end
      
      def car=(car)
        @car = car.nil? ? NULL : car
        car.parent = self if Expression === car and car != NULL
      end
      
      def cdr=(cdr)
        @cdr = cdr.nil? ? NULL : cdr
      end
      
      def clone(&block)
        Cons.construct(self, &block)
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
        raise TypeError.new("Cannot get the length of improper list #{self}") unless tail.cdr == NULL
        size
      end
      alias :size :length
      
      def ==(other)
        return false unless Cons === other
        return false if NULL == other
        @car == other.car and @cdr == other.cdr
      end
      
      def list?
        tail.cdr == NULL; end
      
      def pair?
        not null?; end
      
      def improper?
        pair? and not list?; end
      
      def null?
        self == NULL; end
      
      def to_a
        map { |cell| Cons === cell ? cell.to_a : cell }
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

