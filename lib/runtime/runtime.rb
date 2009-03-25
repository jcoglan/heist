module Heist

  # +Runtime+ objects represent instances of the Heist runtime environment.
  # Each +Runtime+ defines a top-level +Scope+, into which are injected
  # the standard set of primitive functions and special forms as defined
  # in <tt>lib/builtin</tt>.
  #
  # +Runtime+ exposes several methods from the top-level +Scope+ object,
  # allowing runtime objects to be used as interfaces for defining
  # functions, eval'ing code and running source files.
  #
  class Runtime
    
    %w[ data/expression     data/identifier   data/cons
        callable/function   callable/syntax   callable/macro  callable/continuation
        frame               stack             stackless
        scope               binding
        
    ].each do |file|
      require RUNTIME_PATH + file
    end
    
    extend Forwardable
    def_delegators(:@top_level, :[], :eval, :exec, :define, :syntax, :run)
    
    attr_accessor :stack, :top_level
    
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
      
      @top_level = Scope.new(self)
      @stack = stackless? ? Stackless.new : Stack.new
      
      run("#{ BUILTIN_PATH }primitives.rb")
      run("#{ BUILTIN_PATH }syntax.scm")
      run("#{ BUILTIN_PATH }library.scm")
      
      @start_time = Time.now.to_f
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

