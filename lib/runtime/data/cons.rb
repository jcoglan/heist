module Heist
  class Runtime
    
    # +Cons+ is the basic data structure element in Scheme. A +Cons+ is an
    # object with two pointers to other objects, the pointers being called
    # the +car+ and the +cdr+. You can typically think of this type of
    # object as representing a pair of values, though not all +Cons+ objects
    # are pairs according to the <tt>(pair?)</tt> procedure; specifically
    # +NULL+ is not considered a pair.
    #
    # +Cons+ objects can be joined together to form lists; a list is either
    # the empty list or a +Cons+ whose +cdr+ is a list. That is, a list is
    # chain of +Cons+ pairs joined by their +cdr+ fields, whose +car+ fields
    # hold the elements of the list. A list is considered 'proper' if its
    # final +Cons+ is +NULL+, and improper otherwise. Some examples:
    #
    #   Proper list: (1 2 3)    Improper list: (1 2 3 . 4)
    #
    #         #                         #                    # = cons
    #        / \\                      / \\                  / = car
    #       1   #                     1   #                 \\ = cdr
    #          / \\                      / \\               () = NULL
    #         2   #                     2   #
    #            / \\                      / \\
    #           3   ()                    3   4
    #
    # +Cons+ objects are +Enumerable+, though always keep in mind that a
    # +Cons+ does not 'contain' a whole list, it contains one value and a
    # pointer to the rest of the list. Iterating on a +Cons+ involves
    # walking this object graph.

    class Cons
      include Enumerable
      include Expression
      
      attr_accessor :car, :cdr
      
      # +NULL+ is a special +Cons+ instance representing the empty list, which
      # is used as a nil value in Scheme. The empty list is a singleton: there
      # is only ever one empty list in memory, and the Scheme expressions
      # <tt>'()</tt> and <tt>(list)</tt> always return the same object. +NULL+
      # is frozen, and its +car+ and +cdr+ are pointers back to itself.
      NULL = self.new
      NULL.car = NULL.cdr = NULL

      # +NULL+ is only equal to itself.
      def NULL.==(other); equal?(other); end
      def NULL.to_xml(indent = 0); "#{ " " * (4 * indent) }<list />"; end
      NULL.freeze
      
      cadr_combos = (1..4).map { |n|
        (0...(2**n)).map do |x|
          'c' + ("%0#{n}d" % x.to_s(2)).gsub('0', 'a').gsub('1', 'd') + 'r'
        end
      }.flatten
      
      # An array of all the c[ad]+r functions supported by Heist
      ACCESSORS = cadr_combos
      
      class << self
        # Creates a new list from the elements of the enumerable object +enum+,
        # and returns the +Cons+ that forms the head of the list. If +enum+ is
        # empty, +NULL+ is returned. +construct+ optionally takes a block that
        # can be used to transform each value as the list is constructed:
        #
        #   Cons.construct([1,2,3,4]) { |x| x*x }
        #   #=> (1 4 9 16)
        #
        # The method also takes an optional second parameter +linking+, which,
        # if set to true, makes each +car+ element aware of the +Cons+ that
        # holds it. This is used when inlining macro expansions, where
        # expressions need to replace themselves in their parent expression.
        #
        # Note that this method copies improper lists correctly, though
        # iteration over a +Cons+ list does not include the tail value.
        #
        def construct(enum, linking = false, &block)
          return NULL if enum.nil?
          root, last = nil, nil
          enum.each do |value|
            value = block.call(value) if block_given?
            pair = self.new(value)
            pair.hosts(value) if linking
            root ||= pair
            last.cdr = pair if last
            last = pair.tail
          end
          last.cdr = enum.tail.cdr if last and Cons === enum
          root || NULL
        end
        alias :[] :construct
      end
      
      # A +Cons+ is initialized using a +car+ and a +cdr+ value.
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
      
      # Sets the +car+ of the receiving +Cons+, unless it is frozen. If the
      # given value is +nil+, +NULL+ is used instead.
      def car=(car)
        raise ImmutableError.new("Cannot modify list constant") if frozen?
        @car = car.nil? ? NULL : car
      end
      
      # Sets the +cdr+ of the receiving +Cons+, unless it is frozen. If the
      # given value is +nil+, +NULL+ is used instead.
      def cdr=(cdr)
        raise ImmutableError.new("Cannot modify list constant") if frozen?
        @cdr = cdr.nil? ? NULL : cdr
      end
      
      # Recursively freezes the +Cons+ and any conses it points to. This is
      # used to prevent quoted list constants from being modified since they
      # are part of the parse tree: quoting a list does not allocate a new
      # object so changing a quoted list would have undesirable side effects
      # on any other references to the list.
      def freeze!
        return if null?
        [@car, @cdr].each { |slot| slot.freeze! if slot.respond_to?(:freeze!) }
        freeze
      end
      
      # Sets the parent pair of +value+ to the receiver. Some +car+ values
      # need a reference back to their containing +Cons+ for concerns such
      # as macro expansion inlining.
      def hosts(value)
        value.parent = self if Expression === value and value != NULL
      end
      
      # Creates and returns a copy of the +Cons+ list, taking an optional
      # mapping block (see +construct+).
      def clone(&block)
        Cons.construct(self, &block)
      end
      
      # Iterates over each +Cons+ in a list, yielding the +car+ value for
      # each +Cons+. Returns the final +Cons+ in the list, from where we
      # can inspect the tail's +cdr+ to see if the list if proper or not.
      # The block is optional and the method is aliased as +tail+, so to
      # get the tail value of an improper list you'd call <tt>list.tail.cdr</tt>.
      def each
        pair, tail = self, NULL
        while Cons === pair and not pair.null?
          yield(pair.car) if block_given?
          tail = pair
          pair = pair.cdr
        end
        tail
      end
      alias :tail :each
      
      # Returns the length of the list whose head is the receiving +Cons+.
      # If the list is improper an exception is raised.
      def length
        size = 0
        tail = each { |value| size += 1 }
        raise TypeError.new("Cannot get the length of improper list #{self}") unless tail.cdr == NULL
        size
      end
      alias :size :length
      
      # Returns +true+ iff +other+ is equal to the receiver, that is to
      # say that +other+ is a +Cons+ with the same +car+ and +cdr+ as the
      # receiving +Cons+.
      def ==(other)
        return false unless Cons === other
        return false if NULL == other
        @car == other.car and @cdr == other.cdr
      end
      
      # If the receiving list contains +Binding+ objects, said objects are
      # forced to evaluate themselves to populate the list.
      def force!
        return self unless Binding === @car
        pair = self
        while not pair.null?
          pair.car = pair.car.force!
          pair = pair.cdr
        end
        self
      end
      
      # Returns +true+ iff the receiving +Cons+ is the head of a proper list.
      def list?
        tail.cdr == NULL; end
      
      # Returns +true+ iff the receiving +Cons+ is a valid pair.
      def pair?
        not null?; end
      
      # Returns +true+ iff the receiving +Cons+ is the empty list.
      def null?
        self == NULL; end
      
      # Returns an array representation of the list. If +deep+ is +true+,
      # the array conversion is performed recursively.
      def to_a(deep = false)
        map { |cell| deep && Cons === cell ? cell.to_a : cell }
      end
      
      # Returns a pure Ruby representation of the list, with any Heist
      # specific objects converted to basic Ruby equivalents.
      def to_ruby
        map { |cell| cell.respond_to?(:to_ruby) ? cell.to_ruby : cell }
      end
      
      # Returns an XML representation of the list, suitable for enterprise-class
      # turnkey N-tier web architecture deployments, possibly also solutions.
      def to_xml(indent = 0)
        s = " " * (4 * indent)
        "#{s}<pair>\n" +
        Heist.to_xml(@car, indent + 1) + "\n" +
        Heist.to_xml(@cdr, indent + 1) + "\n" +
        "#{s}</pair>"
      end
      
      # Returns a Scheme-style string representation of the list.
      def inspect
        strings = []
        tail = each { |value| strings << value.inspect }.cdr
        '(' + (strings * ' ') +
              (tail == NULL ? '' : ' . ' + tail.to_s) + ')'
      end
      alias :to_s :inspect
    end
    
  end
end

