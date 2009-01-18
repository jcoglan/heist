module Heist
  class Runtime
    
    class Scope
      def initialize(parent)
        @symbols = {}
        return @parent = parent unless Runtime === parent
        @runtime = parent
        @parent  = {}
      end
      
      def runtime
        @runtime || @parent.runtime
      end
      
      def [](name)
        @symbols.has_key?(name) ? @symbols[name] : @parent[name]
      end
      
      def []=(name, value)
        @symbols[name] = value
      end
      
      def define(name, &block)
        @symbols[name.to_s] = Function.new(self, &block)
      end
      
      def metadef(name, &block)
        @symbols[name.to_s] = MetaFunction.new(self, &block)
      end
      
      def eval(source)
        source = Heist.parse(source) if String === source
        source.eval(self)
      end
    end
    
  end
end

