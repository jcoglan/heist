require 'forwardable'

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
        value = @symbols.has_key?(name) ?
                @symbols[name] :
                @parent[name]
        value = value.eval if Binding === value
        value
      end
      
      def []=(name, value)
        @symbols[name] = value
      end
      
      def bind(cells, scope)
        cells.each do |cell|
          self[cell.cells.first.text_value] = cell.cells.last.eval(scope)
        end
      end
      
      def define(name, &block)
        self[name.to_s] = Function.new(self, &block)
      end
      
      def metadef(name, &block)
        self[name.to_s] = MetaFunction.new(self, &block)
      end
      
      def eval(source)
        source = Heist.parse(source) if String === source
        source.eval(self)
      end
      
      def run(path)
        path = path + FILE_EXT unless File.file?(path)
        source = Heist.parse(File.read(path))
        scope = FileScope.new(self, path)
        source.eval(scope)
      end
      
      def load(path)
        dir = load_path.find do |dir|
          File.file?("#{dir}/#{path}") or File.file?("#{dir}/#{path}#{FILE_EXT}")
        end
        return false unless dir
        runtime.run("#{dir}/#{path}")
        true
      end
      
      def current_file
        @path || @parent.current_file rescue nil
      end
      
    private
      
      def load_path
        paths, file = [], current_file
        paths << File.dirname(file) if file
        paths + LOAD_PATH
      end
    end
    
    class FileScope < Scope
      extend Forwardable
      def_delegators(:@parent, :[]=, :eval, :run)
      
      def initialize(parent, path)
        super(parent)
        @path = path
        self['__FILE__'] = path
      end
    end
    
  end
end

