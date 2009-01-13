module Heist
  class Runtime
    
    PATH = File.dirname(__FILE__) + '/runtime/'
    
    %w(function scope thunk).each do |file|
      require PATH + file
    end
    
    attr_reader :order
    
    def initialize(options = {})
      @scope = Scope.new(self)
      
      @order = options[:order] || NORMAL_ORDER
      
      @scope.instance_eval(File.read("#{ PATH }/builtins.rb"))
      @scope.instance_eval(File.read("#{ PATH }/builtins/#{ ORDERS[@order] }.rb"))
      Heist.run("#{ PATH }/builtins.scm", @scope)
      Heist.run("#{ PATH }/builtins/#{ ORDERS[@order] }.scm", @scope)
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

