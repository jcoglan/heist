require 'forwardable'

module Heist
  class Runtime
    
    %w[ expression    list          identifier
        function      macro/macro   continuation
        stack         stackless     frame
        scope         binding
        
    ].each do |file|
      require RUNTIME_PATH + file
    end
    
    extend Forwardable
    def_delegators(:@scope, :[], :eval, :define, :metadef, :call)
    
    attr_reader :order
    attr_accessor :stack
    
    def initialize(options = {})
      @order         = options[:lazy] ? LAZY : EAGER
      @continuations = !!options[:continuations]
      
      @scope = Scope.new(self)
      @stack = create_stack
      
      syntax_type = lazy? ? 'rb' : 'scm'
      
      run("#{ BUILTIN_PATH }primitives.rb")
      run("#{ BUILTIN_PATH }syntax.#{syntax_type}")
      run("#{ BUILTIN_PATH }library.scm")
      
      @start_time = Time.now.to_f
    end
    
    def run(path)
      return instance_eval(File.read(path)) if File.extname(path) == '.rb'
      @scope.run(path)
    end
    
    def elapsed_time
      (Time.now.to_f - @start_time) * 1000000
    end
    
    def lazy?
      @order == NORMAL_ORDER
    end
    
    def stackless?
      lazy? or not @continuations
    end
    
    def create_stack
      stackless? ? Stackless.new : Stack.new
    end
    
  end
end

