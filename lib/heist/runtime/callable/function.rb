module Heist
  class Runtime
    
    # There are several types of callable object in Heist: +Function+, +Syntax+,
    # +Macro+ and +Continuation+. For convenience of type detection, +Function+
    # is treated as a the base class for all these, though they all typically
    # override <tt>Function</tt>'s core methods. The intention is to treat all
    # these types as uniformly as possible, though this does introduce some API
    # problems which still need resolving. In any case, the current state of
    # things:
    #
    #                      ============
    #                      | Function |
    #                      ============
    #                           |
    #               ---------------------------
    #               |                         |
    #          ==========              ================
    #          | Syntax |              | Continuation |
    #          ==========              ================
    #               |
    #           =========
    #           | Macro |
    #           =========
    #
    # Heist is designed to be reasonably modular in that none of Scheme's
    # special forms or built-in functions are mentioned in the parser; instead
    # they are all implemented as first-class functions and stored in the global
    # scope when you initialize a Heist +Runtime+. The Heist runtime does
    # implement a fair amount of Scheme-specific functionality but you're not
    # forced to use any of it: at its core it is designed as a general processor
    # for s-expressions and could be used as a backend for any Lisp-style
    # language. The only hard-and-fast rule is that, to evaluate an expression,
    # you evaluate its first element, and call the resulting function with the
    # remaining elements.
    #
    # The Heist callable types -- +Function+, +Syntax+, +Macro+ and
    # +Continuation+ -- differ primarily in terms of what they do with their
    # arguments when called. All are passed the current +Scope+ and a +Cons+
    # list containing the rest of the current expression when they are called;
    # it is up to the type of function being called what to do with the
    # expression.
    #
    # The +Function+ class is used for representing the basic idea of a Lisp
    # procedure; a reusable block of code that can accept zero, one or more
    # arguments, return output values and cause side effects on the state of the
    # running program. Heist functions can be implemented using Ruby blocks or
    # Scheme lists.
    #
    # For information on other callable types, see +Syntax+, +Macro+ and
    # +Continuation+.
    #
    class Function
      attr_reader :body, :name
      
      # A +Function+ can be initialized in one of two ways. Both types require a
      # +Scope+ as the first parameter: this is the lexical scope in which the
      # +Function+ is defined. The other parameters should consist either of a
      # Ruby block that accepts the procedure's arguments and returns its result:
      #
      #   env = Scope.new
      #   env['plus'] = Function.new(env) { |a,b| a + b }
      #
      # Or, the parameters should be a list of formal argument names for the
      # procedure, and a list containing zero or more Scheme expressions that
      # make up the procedure's body. If the list of formals is an improper
      # list, the tail name will be assigned a list of any parameters remaining
      # after all the preceeding formals have been assigned values. If the list
      # of formals is not a list but a single identifier, that identifier will
      # be assigned a list of all the parameters when the procedure is called.
      #
      def initialize(scope, formals = nil, body = nil, &block)
        @formals, @rest = [], formals
        if Cons === formals
          tail     = formals.tail.cdr
          @formals = formals.map { |id| id.to_s }
          @rest    = (Identifier === tail) ? tail : nil
        end
        @scope = scope
        @body  = body || block
        @lazy  = scope.runtime.lazy?
        @eager = !scope.runtime.stackless?
      end
      
      # Assigns a name to the +Function+, used primarily for console output. A
      # +Function+ keeps the first name it is bound to. This is really only of
      # use to REPL users; functions are first-class data and do not need names
      # in order to operate correctly.
      def name=(name)
        @name ||= name.to_s
      end
      
      # Calls the +Function+ with a calling +Scope+ (in which to evaluate the
      # parameters), and a +Cons+ list containing the parameter expressions. In
      # lazy mode, the parameter expressions are wrapped in +Binding+ objects
      # for later execution. If using the continuation-supporting +Stack+
      # interpreter (as indicated by the <tt>@eager</tt> flag), the parameters
      # will already have been evaluated and should not be re-evaluated lest we
      # try to interpret data lists as code. Returns the result of calling
      # +apply+ with the evaluated parameters.
      def call(scope, cells)
        params = cells.map do |arg|
          lazy? ? Binding.new(arg, scope) :
                  (@eager ? arg : Heist.evaluate(arg, scope))
        end
        apply(params)
      end
      
      # Returns the result of applying the +Function+ to the given collection of
      # +params+. If the procedure is primitive, this will return a value
      # immediately. If it's a Scheme function, this simply binds the +params+
      # to the procedure's formal arguments and returns a +Body+ with the
      # procedure's body expressions and the newly allocated +Scope+. We use a
      # trampoline to call <tt>Function</tt>s iteratively to allow tail call
      # optimisation. (See +Stackless+, +Stack+, +Frame+ and +Body+ for more
      # information.)
      def apply(params)
        return @body.call(*params) if primitive?
        closure = Scope.new(@scope)
        idx = 0
        @formals.each do |name|
          closure[name] = params[idx]
          idx += 1
        end
        closure[@rest] = Cons.construct(params[idx..-1]) if @rest
        Body.new(@body, closure)
      end
      
      # Returns +true+ iff the +Function+ is a primitive, that is to say it is
      # implemented in Ruby rather than Scheme.
      def primitive?
        Proc === @body
      end
      
      # Returns +true+ iff the +Function+ is lazy, i.e. it does not evaluate its
      # arguments unless it needs their values. Only non-primitive functions can
      # be lazy since we have no way of telling when Ruby needs to access a
      # value. Besides, primitive functions will typically use all their
      # parameters.
      def lazy?
        @lazy and not primitive?
      end
      
      # Returns a string placeholder for the +Function+, containing its name if
      # it has one.
      def to_s
        "#<procedure:#{ @name }>"
      end
      alias :inspect :to_s
    end
    
  end
end

