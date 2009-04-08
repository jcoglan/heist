module Heist
  class Runtime
    
    # The +Macro+ class is a type of +Function+ (it inherits from +Syntax+ to
    # let the evaluator know it consumes syntax rather than values) that is
    # used to represent non-primitive syntax as defined by the Scheme
    # <tt>(syntax-rules)</tt> macro system. Calling a +Macro+ involves passing
    # it an input +Expression+, which it will either convert into another
    # +Expression+ according to user-defined rules, or it will raise an
    # exception if the input is syntactically invalid. On success, a +Macro+
    # will return an +Expansion+, which contains an +Expression+ that can be
    # directly evaluated or expanded further.
    #
    # Heist does not have distinct macro expansion and runtime phases; its
    # macros are first-class runtime objects and all expansion takes place
    # as macros are encountered while the program is running. Once a macro
    # call is processed, the expansion is inlined into the source tree,
    # replacing the macro call to avoid further unnecessary expansions.
    #
    # +Macro+ uses several auxiliary classes to do its work. See +Matches+,
    # +Tree+ and +Expansion+ for more information.
    #
    class Macro < Syntax
      
      # The ellipsis identifier, used to indicate repeated patterns
      ELLIPSIS = Identifier.new('...')
      
      # Array of reserved symbols that cannot be used as pattern vars
      RESERVED = %w[_ ...]
      
      %w[expansion matches tree].each do |klass|
        require RUNTIME_PATH + 'callable/macro/' + klass
      end
      
      # Takes an s-expression and returns an array of the pattern variables
      # it contains. The optional second argument should be an array, and
      # specifies a list of names to exclude, for example to exclude the formal
      # keywords of a macro transformer. The final argument is for internal use
      # only; we call this method recursively but pass down a single array to
      # push results onto to avoid lots of array joins.
      def self.pattern_vars(pattern, excluded = [], results = [])
        return results if Cons::NULL == pattern
        case pattern
          when Identifier then
            name = pattern.to_s
            return if excluded.include?(name) or RESERVED.include?(name)
            results << name unless results.include?(name)
          when Cons then
            tail = pattern.each { |cell| pattern_vars(cell, excluded, results) }
            pattern_vars(tail.cdr, excluded, results)
        end
        results
      end
      
      # Calls the +Macro+ with the current +Scope+ and the +Cons+ list of the
      # rest of the expression, i.e. the syntax tree for the macro to operate
      # on. Returns an +Expansion+ if the given syntax is found to be valid,
      # otherwise raises an exception.
      def call(scope, cells)
        rule, matches = *rule_for(cells, scope)
        return Expansion.new(@scope, scope, rule.cdr.car, matches) if rule
        raise SyntaxError.new(
          "Bad syntax: no macro expansion found for #{Cons.new(@name, cells)}")
      end
      
      # Returns a string placeholder for the +Macro+, containing its
      # name if it has one.
      def to_s
        "#<macro:#{ @name }>"
      end
      alias :inspect :to_s
      
      # Takes a +Cons+ expression and a +Scope+ (required for determining
      # the binding of macro keywords), and returns a tuple containing an
      # expansion template and a set of pattern match data for the first
      # rule in the macro that matches the input. If no such rule is found,
      # returns +nil+.
      def rule_for(cells, scope)
        @body.each do |rule|
          matches = rule_matches(scope, rule.car.cdr, cells)
          return [rule, matches] if matches
        end
        return nil
      end
      
      # Takes a +Scope+ (the scope from which the macro is being called,
      # required for determining keyword bindings), an +Expression+
      # representing a macro pattern, and an input +Expression+ from
      # the expression the macro was called with. If the pattern matches
      # the input, a +Matches+ object is returned, otherwise we return
      # +nil+. The +matches+ and +depth+ arguments are for internal use
      # only and are passed down as the match algorithm recurses over
      # the pattern and input expressions.
      #
      # +matches+ is a +Matches+ instance that stores data about which
      # input expressions correspond to which pattern variables, and how
      # often they repeat. +depth+ indicates the repetition depth, that
      # is how many ellipses appear following the current pattern.
      #
      # From the R5RS spec
      # http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html
      #
      # More formally, an input form F matches a pattern P if and only if:
      #
      # * P is a non-literal identifier; or
      # * P is a literal identifier and F is an identifier with the
      #   same binding; or
      # * P is a list (P1 ... Pn) and F is a list of n forms that match
      #   P1 through Pn, respectively; or
      # * P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list
      #   or improper list of n or more forms that match P1 through Pn,
      #   respectively, and whose nth 'cdr' matches Pn+1; or
      # * P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis>
      #   is the identifier '...' and F is a proper list of at least n forms,
      #   the first n of which match P1 through Pn, respectively, and
      #   each remaining element of F matches Pn+1; or
      # * P is a vector of the form #(P1 ... Pn) and F is a vector of n
      #   forms that match P1 through Pn; or
      # * P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis>
      #   is the identifier '...' and F is a vector of n or more forms the
      #   first n of which match P1 through Pn, respectively, and each
      #   remaining element of F matches Pn+1; or
      # * P is a datum and F is equal to P in the sense of the 'equal?'
      #   procedure.
      #
      # It is an error to use a macro keyword, within the scope of its
      # binding, in an expression that does not match any of the patterns.
      #
      def rule_matches(scope, pattern, input, matches = nil, depth = 0)
        matches ||= Matches.new(pattern, @formals)
        case pattern
        
          when Cons then
            # If pattern is NULL, the input must also be NULL
            return (Cons::NULL == input ? matches : nil) if pattern.null?
            
            # Fail if the pattern is a list and the input is not
            return nil unless Cons === input
            
            # Iterate over the pattern, consuming input as we go
            pattern_pair, input_pair = pattern, input
            skip = lambda { pattern_pair = pattern_pair.cdr }
            
            while Cons === pattern_pair and not pattern_pair.null?
              token = pattern_pair.car
              
              # Skip the current pattern token if it's an ellipsis
              skip[] and next if token == ELLIPSIS
              
              # Increment the repetition depth if the next pattern
              # token is an ellipsis, and inform the +Matches+ object
              # that the pattern vars in the current pattern have
              # hit a repetition boundary. Note we do not increment
              # +depth+ itself since this would persist for the remaining
              # tokens in the pattern after we get past the ellipsis.
              followed_by_ellipsis = (pattern_pair.cdr.car == ELLIPSIS rescue false)
              dx = followed_by_ellipsis ? 1 : 0
              
              matches.descend!(Macro.pattern_vars(token, @formals),
                               depth + dx) if followed_by_ellipsis
              
              # Set up a closure to consume input using the current
              # pattern expression. Calls +rule_matches+ with the
              # current scope, pattern, input, and +Matches+ object.
              consume = lambda do
                Cons === input_pair and not input_pair.null? and
                rule_matches(scope, token, input_pair.car, matches, depth + dx)
              end
              
              # If the next pattern token is not an ellipsis, fail
              # unless the pattern token matches the input token.
              #
              # If the next token is an ellipsis, consume input
              # using the current pattern until the pattern no
              # longer matches the current input
              #
              return nil unless consume[] or followed_by_ellipsis
              input_pair = input_pair.cdr
              input_pair = input_pair.cdr while followed_by_ellipsis and consume[]
              
              skip[]
            end
            
            # We're done iterating over the pattern, so the current
            # pattern token will be NULL or some non-Cons object (if
            # the pattern is an improper list). Fail unless the remaining
            # input matches this object.
            return nil unless rule_matches(scope, pattern_pair, input_pair, matches, depth)
        
          # If the pattern is a formal keyword for the macro (a
          # 'literal identifier' in the terms of the spec), return
          # a boolean indicating whether the input is an identifier
          # with the same binding, that is to say the two identifiers
          # refer to the same location in memory (or both refer to
          # no location). If it's a normal pattern variable, store
          # the current input, whatever it is, in the +matches+.
          when Identifier then
            if @formals.include?(pattern.to_s)
              return pattern == input && @scope.innermost_binding(pattern) ==
                                         scope.innermost_binding(input)
            else
              matches.put(pattern, input)
            end
        
          # If all above type checks on the pattern fail, assume the
          # pattern is literal data and make sure the input matches.
          else
            return pattern == input ? matches : nil
        end
        matches
      end
      
    end
    
  end
end

