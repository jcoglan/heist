module Heist
  class Runtime
    class Macro
      
      # +Matches+ instances, with help from the +Tree+ class, are data
      # structures that represent the way in which syntactic expressions match
      # patterns found in +Macro+ rules. They provide an API for storing and
      # retrieving such data, with the aim of removing some clutter from the
      # macro parsing and expansion routines.
      #
      # At a pure Ruby level, a +Matches+ is a wrapper around a hash that maps
      # pattern variables to +Tree+ objects, which themselves are wrappers
      # around nested arrays that represent how repeated pattern matches are
      # grouped together. For example, given the pattern
      #
      #   (do ([variable init step ...] ...)
      #       (test expression ...)
      #       command ...)
      #
      # and the expression
      #
      #   (do ([x 6 (- x 1)]
      #        [acc 1])
      #       ((zero? x) acc)
      #     (display x) (newline)
      #     (set! acc (* acc x)))
      #
      # the +Matches+ object would contain the following:
      #
      #   @data = {
      #       "variable"   => [ x,
      #                         acc
      #                       ],
      #       
      #       "init"       => [ 6,
      #                         1
      #                       ],
      #       
      #       "step"       => [ [ (- x 1)
      #                         ],
      #                         []
      #                       ],
      #       
      #       "test"       => (zero? x),
      #       
      #       "expression" => [ acc
      #                       ],
      #       
      #       "command"    => [ (display x),
      #                         (newline),
      #                         (set! acc (* acc x))
      #                       ]
      #   }
      #
      # Breaking this down, we see +test+ is not followed by an ellipsis in the
      # pattern, and can thus only consume one item from the input expression.
      # So, its match data is a single +Expression+. +variable+, +init+,
      # +command+ and +expression+ are all followed by a single ellipsis
      # (+variable+ and +init+ appear in a list that is followed by an ellipsis),
      # so can consume several values each; their match data are arrays of
      # expressions.
      #
      # +step+, on the other hand, is followed by two ellipses: it itself is
      # followed by an ellipsis, and <tt>step ...</tt> appears inside a list
      # that is also followed by an ellipsis. If a pattern is followed by more
      # than one ellipsis, the match data it generates is a tree of nested
      # arrays that describe how the expressions are grouped. Here, we see that
      # the expression <tt>[variable init step ...]</tt> appears twice in the
      # input, so +step+'s root match element is an array of two elements. But,
      # +step+ does not match any data in the second appearance
      # (<tt>[acc 1]</tt>), so the second element of this array is empty. The
      # first element is an array containing the single match from the first
      # appearance (<tt>(- x 1)</tt> in the expression <tt>[x 6 (- x 1)]</tt>).
      #
      # +Matches+ tries to hide many of these details so the macro routines can
      # read and write to this data structure in the simplest possible terms.
      #
      class Matches
        
        # A +Matches+ is initialized using a +Cons+ representing a macro pattern,
        # and an array of formal keywords supplied by the +Macro+. Keywords do
        # not need to store matches so they are ignored here.
        def initialize(pattern, formals)
          @data = {}
          names = Macro.pattern_vars(pattern, formals)
          names.each { |name| @data[name] = Tree.new(name) }
        end
        
        # Tells the +Matches+ object that the given pattern variables (the array
        # +names+) have encountered a trailing ellipsis at the given repetition
        # depth. This allows +Matches+ to group repeated patterns correctly.
        def descend!(names, depth)
          @data.each do |name, set|
            set.descend!(depth) if names.include?(name)
          end
        end
        
        # Writes an expression to the +Matches+ object under the variable +name+.
        # The receiver deals with storing it in the correct repetition group.
        def put(name, value)
          name = name.to_s
          @data[name] << value if has?(name)
        end
        
        # Returns +true+ iff the receiver has a pattern variable named +name+.
        def has?(name)
          @data.has_key?(name.to_s)
        end
        
        # Retrieves an expression from the +Matches+ under the given +name+. The
        # receiver deals with pulling the expression from the right point in the
        # tree; see the <tt>expand!</tt>, <tt>iterate!</tt> and
        # <tt>Tree#read</tt> and <tt>Tree#shift!</tt> methods.
        def get(name)
          @data[name.to_s].read
        end
        
        # Takes a +template+ +Expression+, a repetition +depth+ and a block, and
        # calls the block +n+ times, where +n+ is the number of matches for the
        # pattern variables in the template at the given depth and the current
        # iteration point in the tree. After each block call, the +Matches+
        # object moves the pointer for all the applicable pattern variables
        # along one place at the given depth -- see <tt>iterate!</tt> and
        # <tt>Tree#shift!</tt>.
        def expand!(template, depth)
          names = Macro.pattern_vars(template)
          size(names, depth).times { yield() and iterate!(names, depth) }
        end
        
      private
        
        # Returns the number of matched expressions available for the given set
        # of pattern variables at the given depth, at the current iteration
        # point. An exception is raised if the names do not all yield the same
        # number of matches; this indicates a piece of mismatched syntax that
        # cannot be expanded correctly.
        def size(names, depth)
          sizes = []
          @data.each do |name, tree|
            sizes << tree.size(depth) if names.include?(name)
          end
          
          sizes = sizes.compact.uniq
          return sizes.first if sizes.size == 1
          
          raise MacroTemplateMismatch.new(
            "Macro could not be expanded: mismatched repetition patterns")
        end
        
        # Shifts the tree pointer (see <tt>Tree#shift!</tt> for all the given
        # +names+ along one place at the given +depth+. This is used while
        # expanding repeated patterns using <tt>expand!</tt>.
        def iterate!(names, depth)
          @data.each do |name, tree|
            tree.shift!(depth) if names.include?(name)
          end
        end
      end
      
    end
  end
end

