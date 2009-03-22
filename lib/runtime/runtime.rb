module Heist
  class Runtime
    
    %w[ data/expression     data/identifier   data/cons
        callable/function   callable/macro    callable/continuation
        frame               stack             stackless
        scope               binding
        
    ].each do |file|
      require RUNTIME_PATH + file
    end
    
    extend Forwardable
    def_delegators(:@top_level, :[], :eval, :exec, :define, :syntax, :call, :run)
    
    attr_reader :order
    attr_accessor :stack, :top_level
    
    def initialize(options = {})
      @lazy          = !!options[:lazy]
      @continuations = !!options[:continuations]
      @hygienic      = !options[:unhygienic]
      
      @top_level = Scope.new(self)
      @stack = create_stack
      
      run("#{ BUILTIN_PATH }primitives.rb")
      run("#{ BUILTIN_PATH }syntax.scm")
      run("#{ BUILTIN_PATH }library.scm")
      
      @start_time = Time.now.to_f
    end
    
    def elapsed_time
      (Time.now.to_f - @start_time) * 1000000
    end
    
    def lazy?; @lazy; end
    
    def hygienic?; @hygienic; end
    
    def stackless?
      lazy? or not @continuations
    end
    
    def create_stack
      stackless? ? Stackless.new : Stack.new
    end
    
    def to_s
      "#<runtime: #{ stackless? ? 'call/cc disabled' : 'call/cc enabled'
               }, #{ hygienic? ? 'hygienic' : 'unhygienic'
               }, #{ lazy? ? 'lazy' : 'eager' }>"
    end
    alias :inspect :to_s
    
  end
end

