module Heist

  # +Runtime+ objects represent instances of the Heist runtime environment.
  # Each +Runtime+ defines a top-level +Scope+, into which are injected
  # the standard set of primitive functions and special forms as defined
  # in <tt>lib/builtin</tt>.
  #
  # User code runs in another scope descended from the top-level. This is
  # done so that user-level redefinitions of built-in functions do not break
  # other built-in functions that refer to the redefined names; lexical
  # scoping ensures that built-in functions can only refer to other bindings
  # in the top-level scope so user-level bindings do not affect the built-in
  # library functions.
  #
  # +Runtime+ exposes several methods from the user-level +Scope+ object,
  # allowing runtime objects to be used as interfaces for defining
  # functions, eval'ing code and running source files.
  #
  class Runtime
    
    %w[ data/expression     data/identifier   data/character
        data/cons           data/vector
        callable/function   callable/syntax   callable/macro  callable/continuation
        frame               stack             stackless
        scope               binding
        
    ].each do |file|
      require RUNTIME_PATH + file
    end
    
    extend Forwardable
    def_delegators(:@user_scope, :[], :eval, :exec, :program, :define, :syntax, :run, :load)
    
    attr_accessor :stack, :top_level, :user_scope
    
    BUILTIN_LIBRARIES = %w[primitives syntax library]
    
    # A +Runtime+ is initialized using a set of options. The available
    # options include the following, all of which are +false+ unless
    # you override them yourself:
    #
    # * <tt>:continuations</tt>: set to +true+ to enable <tt>call/cc</tt>
    # * <tt>:lazy</tt>: set to +true+ to enable lazy evaluation
    # * <tt>:unhygienic</tt>: set to +true+ to disable macro hygiene
    #
    def initialize(options = {})
      @lazy          = !!options[:lazy]
      @continuations = !!options[:continuations]
      @hygienic      = !options[:unhygienic]
      
      @top_level  = Scope.new(self)
      @user_scope = Scope.new(@top_level)
      @stack      = stackless? ? Stackless.new : Stack.new
      
      load_builtins(options)
      @start_time = Time.now.to_f
    end
    
    # To stop user-space redefinitions of built-in functions from breaking
    # the standard library, we define builtins in a privileged scope, one up
    # from the scope that user code runs in. We then bind the names in 'user
    # space' to stop (set!) from reaching into our privileged top level.
    def load_builtins(options = {})
      libraries = (options[:only] || BUILTIN_LIBRARIES) - (options[:except] || [])
      
      libraries.each do |library|
        @top_level.run(@top_level.expand_path(library))
      end
      @user_scope.each_var(&@user_scope.method(:[]=))
    end
    
    # Returns the length of time the +Runtime+ has been alive for, as a
    # number in microseconds.
    def elapsed_time
      (Time.now.to_f - @start_time) * 1000000
    end
    
    # Returns +true+ iff the +Runtime+ is using lazy evaluation.
    def lazy?; @lazy; end
    
    # Returns +true+ iff the +Runtime+ is using hygienic macros.
    def hygienic?; @hygienic; end
    
    # Returns +true+ iff the +Runtime+ is using the faster +Stackless+
    # evaluator, which does not support <tt>(call/cc)</tt>.
    def stackless?
      lazy? or not @continuations
    end
    
    def to_s
      "#<runtime: #{ stackless? ? 'call/cc disabled' : 'call/cc enabled'
               }, #{ hygienic? ? 'hygienic' : 'unhygienic'
               }, #{ lazy? ? 'lazy' : 'eager' }>"
    end
    alias :inspect :to_s
    
    def info
      [ "Heist Scheme interpreter v. #{ VERSION }",
        "Evaluation mode: #{ lazy? ? 'LAZY' : 'EAGER' }",
        "Continuations enabled? #{ stackless? ? 'NO' : 'YES' }",
        "Macros: #{ hygienic? ? 'HYGIENIC' : 'UNHYGIENIC' }\n\n"
      ] * "\n"
    end
    
  end
end

