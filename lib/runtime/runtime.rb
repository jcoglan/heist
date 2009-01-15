module Heist
  class Runtime
    
    %w(function scope thunk).each do |file|
      require RUNTIME_PATH + file
    end
    
    attr_reader :order
    
    def initialize(options = {})
      @scope = Scope.new(self)
      
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

