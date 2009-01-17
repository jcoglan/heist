require 'forwardable'

module Heist
  class Runtime
    
    %w(frame function scope binding).each do |file|
      require RUNTIME_PATH + file
    end
    
    extend Forwardable
    def_delegators(:@scope, :[], :eval, :define, :metadef)
    
    attr_reader :order, :stack
    
    def initialize(options = {})
      @scope = Scope.new(self)
      @stack = []
      
      @order = options[:order] || EAGER
      
      instance_eval(File.read("#{ BUILTIN_PATH }common.rb"))
      instance_eval(File.read("#{ BUILTIN_PATH }#{ ORDERS[@order] }.rb"))
      Heist.run("#{ BUILTIN_PATH }common.scm", @scope)
      Heist.run("#{ BUILTIN_PATH }#{ ORDERS[@order] }.scm", @scope)
    end
    
    def lazy?
      @order == NORMAL_ORDER
    end
    
  end
end

