module Heist
  class Runtime
    
    class Scope
      def initialize(parent)
        @symbols = {}
        return @parent = parent unless Runtime === parent
        @runtime = parent
        @parent = {}
      end
      
      def runtime
        @runtime || @parent.runtime
      end
      
      def [](name)
        name = name.to_s
        value = @symbols.has_key?(name) ?
                @symbols[name] :
                @parent[name]
        value = value.extract if Binding === value
        value
      end
      
      def []=(name, value)
        @symbols[name.to_s] = value
      end
      
      def bind(list, scope)
        list.each do |list|
          self[list.first.to_s] = Heist.value_of(list.last, scope)
        end
      end
      
      def define(name, *args, &block)
        self[name] = Function.new(self, *args, &block)
      end
      
      def metadef(name, &block)
        self[name] = MetaFunction.new(self, &block)
      end
      
      def run(path)
        path = path + FILE_EXT unless File.file?(path)
        source = Heist.parse(File.read(path))
        source.eval(self)
      end
      
      def eval(source)
        source = Heist.parse(source) if String === source
        source.eval(self)
      end
    end
    
  end
end

