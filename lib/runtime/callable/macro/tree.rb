module Heist
  class Runtime
    class Macro
      
      # <tt>Tree</tt>s are used by instances of +Matches+ to store expressions
      # matched by macro patterns. Patterns may contain patterns that repeat
      # (indicated by following the pattern with an ellipsis), and these repetitions
      # may be nested. +Tree+ instances store expressions in a set of nested arrays
      # that match the repetition structure of the macro pattern being matched.
      # See +Matches+ for a fuller explanation.
      #
      # Every +Tree+ contains an array called <tt>@data</tt> that stores the
      # expressions matched by a pattern variable, a variable <tt>@depth</tt> that
      # stores the maximum repetition depth of the tree (a non-repeated pattern
      # has depth zero), and an array <tt>@indexes</tt> which is used to maintain
      # a list of array indexes that point to the current read position in the
      # tree while a macro is being expanded. For example, taking the pattern from
      # the +Matches+ example:
      #
      #   (do ([variable init step ...] ...)
      #       (test expression ...)
      #       command ...)
      #
      # Say we had the following expression (not entirely valid (do) syntax, but
      # compatible with the above pattern):
      #
      #   (do ([x 6 (- x 1) (- acc 1)]
      #        [y 5]
      #        [acc 1 (* x acc)])
      #       ((zero? x) acc)
      #     (display x) (newline)
      #     (set! acc (* acc x)))
      #
      # The resulting +Matches+ object would contain the following data for the
      # variable +step+:
      #
      #   "step" => [ [ (- x 1),
      #                 (- acc 1)
      #               ],
      #               
      #               [],
      #               
      #               [ (* x acc)
      #               ]
      #             ]
      #
      # That is, the outermost repetition <tt>[variable init step ...]</tt>
      # occurs three times; the first appearance includes two matches for
      # <tt>step ...</tt>, the second no matches and the third one match. With
      # this data, an <tt>@indexes</tt> state of <tt>[0,0]</tt> would read
      # <tt>(- x 1)</tt>, a state of <tt>[0,1]</tt> would read <tt>(- acc 1)</tt>,
      # and <tt>[2,0]</tt> would read <tt>(* x acc)</tt>; the latter instructing
      # the +Tree+ to get the third element of the root array, then the first
      # element of _that_ array to find the right value.
      #
      # In practise, all +Tree+ objects have an extra array around the data as
      # presented above, to make the no-repetition case consistent with the
      # representation for arbitrarily nested repetitions. That is, the methods
      # in this class expect to read from an array in general, so the representation
      # of a non-repeating pattern is just a single-element array to simplify
      # the implementation of these methods in the general case. The first item
      # in the <tt>@indexes</tt> array is always zero. We could remove this extra
      # container and add a type check on <tt>@data</tt> when reading, but the
      # current implementation seems more elegant for the moment.
      #
      class Tree
        
        # A +Tree+ is initialized using the name of the pattern variable it is
        # associated with (for debugging purposes).
        def initialize(name)
          @name  = name
          @data  = []
          @depth = 0
        end
        
        # Tells the receiving +Tree+ that its pattern variable has been visited
        # at a repetition depth of +depth+ during pattern matching. This allocates
        # a new empty array at an appropriate place in the tree to store matches
        # (or groups of matches) if any are encountered. Calls to this method are
        # also used to determine the tree's maximum depth.
        def descend!(depth)
          tail(depth-1) << []
          @depth = depth if depth > @depth
        end
        
        # Pushes an expression onto the end of the final branch of the tree. All
        # expressions should exist at the same depth (the tree's maximum depth),
        # seeing as the pattern should be followed by the same number of ellipses
        # every time it is encountered.
        def <<(value)
          tail(@depth) << value
        end
        
        # Returns the expression at the current read position as instructed by
        # the <tt>@indexes</tt> list.
        def read
          current(@depth)[indexes[@depth]]
        end 
        
        # Shifts the read position at the given +depth+ along by one, by adding 1
        # to one of the values in <tt>@indexes</tt>. The macro expander calls this
        # while walking a template to iterate over repetition branches.
        def shift!(depth)
          return if depth > @depth
          indexes[depth] += 1
          indexes[depth] = 0 if indexes[depth] >= current(depth).size
        end
        
        # Returns the number of matches (or groups of matches) on the current
        # read branch at the given +depth+. Returns zero if no branch exists at
        # the given indexes.
        def size(depth)
          return nil if depth > @depth
          current(depth).size rescue 0
        end
        
      private
        
        # Returns the rightmost branch of the tree at the given +depth+. Used
        # when allocating new branches as repetition blocks are entered.
        def tail(depth)
          (0...depth).inject(@data) { |list, d| list.last }
        end
        
        # Returns the current read branch at the given +depth+, as instructed
        # by the <tt>@indexes</tt> list.
        def current(depth)
          indexes[0...depth].inject(@data) { |list, i| list[i] }
        end
        
        # Initializes the <tt>@indexes</tt> list once the maximum depth is
        # known, and returns the list thereafter.
        def indexes
          @indexes ||= (0..@depth).map { 0 }
          @indexes
        end
      end
      
    end
  end
end

