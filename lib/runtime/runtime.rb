module Heist
  class Runtime
    
    %w(function scope thunk).each do |file|
      require RUNTIME_PATH + file
    end
    
    if LOG_STACK
      [Function, MetaFunction].each do |klass|
        old_call = klass.instance_method(:call)
        klass.__send__(:define_method, :call) do |scope, *args|
          begin
            puts scope.runtime.stack.size
            scope.runtime.stack << self
            result = old_call.bind(self)[scope, *args]
            scope.runtime.stack.pop
            result
          rescue
            scope.runtime.stack.pop
          end
        end
      end
    end
    
    attr_reader :order, :stack
    
    def initialize(options = {})
      @scope = Scope.new(self)
      @stack = []
      
      @order = options[:order] || EAGER
      
      @scope.instance_eval(File.read("#{ BUILTIN_PATH }common.rb"))
      @scope.instance_eval(File.read("#{ BUILTIN_PATH }#{ ORDERS[@order] }.rb"))
      Heist.run("#{ BUILTIN_PATH }common.scm", @scope)
      Heist.run("#{ BUILTIN_PATH }#{ ORDERS[@order] }.scm", @scope)
    end
    
    def lazy?
      @order == NORMAL_ORDER
    end
    
    def [](name)
      @scope[name]
    end
    
    def eval(source)
      @scope.eval(source)
    end
    
  end
end

