module Heist
  class Runtime
    
    PATH = File.dirname(__FILE__) + '/runtime/'
    
    %w(function scope reference builtins).each do |file|
      require PATH + file
    end
    
    def initialize
      @scope = Scope.new
      Builtins.add(@scope)
    end
    
    def eval(source)
      @scope.eval(source)
    end
    
  end
end

