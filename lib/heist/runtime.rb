module Heist
  class Runtime
    
    PATH = File.dirname(__FILE__) + '/runtime/'
    
    %w(function scope thunk).each do |file|
      require PATH + file
    end
    
    def initialize
      @scope = Scope.new
      @scope.instance_eval(File.read("#{ PATH }/builtins.rb"))
      @scope.instance_eval(File.read("#{ PATH }/builtins/#{ EVAL_MODE }.rb"))
      Heist.run("#{ PATH }/builtins.scm", @scope)
      Heist.run("#{ PATH }/builtins/#{ EVAL_MODE }.scm", @scope)
    end
    
    def [](name)
      @scope[name]
    end
    
    def eval(source)
      @scope.eval(source)
    end
    
  end
end

