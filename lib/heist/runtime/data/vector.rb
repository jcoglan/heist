module Heist
  class Runtime
    
    # +Vector+ is Scheme's equivalent of Ruby's +Array+ class, and Heist
    # implements it by subclassing +Array+. In Scheme code a vector is denoted
    # as a proper list preceeded by a <tt>#</tt> symbol, for example
    # <tt>#(1 2 3)</tt>. Vectors are flat, non-recursive data structures,
    # meaning they are faster for accessing slots by numeric index since we do
    # not have to walk a tree to find the correct node.
    #
    # As an example, you can think of the list <tt>(1 2 3)</tt> and the vector
    # <tt>#(1 2 3 4)</tt> as having the following structure:
    #
    #   Proper list: (1 2 3)      Vector: #(1 2 3 4)
    #
    #         .                           .
    #        / \                          |
    #       1   .                   -------------
    #          / \                  |   |   |   |
    #         2   .                 1   2   3   4
    #            / \ 
    #           3  ()
    #
    # Accessing a member of a vector takes time independent of the member's
    # index, whereas access time scales as O(n) for lists.
    #
    class Vector < Array
      # A +Vector+ is initialized using a sequence of values, just like a Ruby
      # +Array+. Optionally, it can be initialized using an array and a block,
      # which will be used to map the array to new values before inserting into the +Vector+.
      # 
      #   Vector.new([1,2,3,4]) { |x| x*x }
      #   #=> #(1 2 3 4)
      #
      def initialize(*args, &block)
        return super(*args) unless block_given?
        args.first.each_with_index { |cell, i| self[i] = block.call(cell) }
      end
      
      # Performs a recursive freeze of the +Vector+ and all its members.
      def freeze!
        freeze
        each { |slot| slot.freeze! if slot.respond_to?(:freeze!) }
      end
      
      # Sets the +value+ at the given +index+. Will throw an exception if the
      # +Vector+ is frozen. The Scheme function <tt>(vector-set!)</tt> also
      # performs out-of-bounds checks to keep vectors a fixed size, but this is
      # not enforced at this level.
      def []=(index, value)
        raise ImmutableError.new("Cannot modify vector constant") if frozen?
        super
      end
      
      # Returns a Scheme-style string representation of the +Vector+.
      def to_s
        '#(' + map { |cell| Heist.stringify(cell) }.join(' ') + ')'
      end
      alias :inspect :to_s
    end
    
  end
end

