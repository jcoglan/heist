module Heist
  class Runtime
    
    class Scope
      def initialize(parent = nil)
        @symbols = {}
        return @parent = parent unless Runtime === parent
        @runtime = parent
        @parent = {}
      end
      
      def runtime
        @runtime || @parent.runtime
      end
      
      def [](name)
        name = to_name(name)
        value = binds?(name) ?
                @symbols[name] :
                @parent[name]
        value = value.extract if Binding === value
        value
      end
      
      def []=(name, value)
        @symbols[to_name(name)] = value
      end
      
      def binds?(name)
        @symbols.has_key?(to_name(name))
      end
      
      def set(name, value)
        return self[name] = value if binds?(name)
        @parent[name] = value if Scope === @parent
      end
      
      def define(name, *args, &block)
        self[name] = Function.new(self, *args, &block)
      end
      
      def metadef(name, &block)
        self[name] = MetaFunction.new(self, &block)
      end
      
      # TODO: this isn't great, figure out a way for functions
      # to transparently handle inter-primitive calls so Ruby can
      # call Scheme code as well as other Ruby code
      def call(name, *params)
        self[name].body.call(*params)
      end
      
      def run(path)
        path   = path + FILE_EXT unless File.file?(path)
        source = Heist.parse(File.read(path))
        scope  = FileScope.new(self, path)
        source.eval(scope)
      end
      
      def eval(source)
        source = Heist.parse(source) if String === source
        source.eval(self)
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
      
      def to_name(name)
        name.to_s.downcase
      end
      
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

