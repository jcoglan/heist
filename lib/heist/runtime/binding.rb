module Heist
  class Runtime
    
    # A +Binding+ is a simple data object that couples an +Expression+ to a
    # +Scope+ so the expression can be evaluated at some later time. Evaluation
    # is typically memoized, though this is not always the case.
    # <tt>Binding</tt>s are analogous to the notion of 'promises' in Scheme,
    # though in Heist promises are implemented (as suggested in R5RS) using a
    # <tt>(delay)</tt> macro that produces memoized closures. <tt>Binding</tt>s
    # are used to implement lazy evaluation, and to maintain lexical scope in
    # hygienic macro expansions by tying variables in the expansion to the
    # lexical scope of the macro.
    #
    # The +Binding+ class mixes in the +Expression+ module purely as a flag to
    # indicate to the evaluator that it can be evaluated.
    #
    class Binding
      include Expression
      
      attr_reader :expression, :scope
      
      # To initialize a +Binding+ supply an +Expression+ and a +Scope+ in which
      # to evaluate it. An optional third argument states whether the evaluation
      # should be memoized.
      def initialize(expression, scope, memoized = true)
        @expression = expression
        @scope      = scope
        @memoized   = !!memoized
      end
      
      # Evaluates the +Binding+ and returns the result. The +Expression+ is only
      # ever evaluated once if the +Binding+ has been memoized.
      def force!
        return @value if defined?(@value) and @memoized
        @value = Heist.evaluate(@expression, @scope)
      end
      
      # This method is provided as a convenience so that a +Binding+ may be
      # treated like any other expression during evaluation. All it does is
      # return the result of calling <tt>force!</tt>.
      def eval(scope)
        force!
      end
      
      # We provide an equality method so that a bound +Identifier+ produced by
      # expanding a macro can be matched against literal identifiers in another
      # macro pattern.
      def ==(identifier)
        @expression == identifier
      end

      def innermost_binding(identifier)
        @scope
      end
      
      # Returns a Rubyish representation of the binding's expression.
      def to_ruby
        @expression.respond_to?(:to_ruby) ? @expression.to_ruby : @expression
      end

      # Returns a string representation of the binding's +Expression+.
      def to_s
        @expression.to_s
      end
      alias :inspect :to_s
    end
    
  end
end

